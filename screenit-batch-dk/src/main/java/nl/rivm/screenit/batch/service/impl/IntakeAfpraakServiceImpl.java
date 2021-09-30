
package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.dao.IntakeAfspraakDao;
import nl.rivm.screenit.batch.model.ClientAfspraak;
import nl.rivm.screenit.batch.service.IntakeAfpraakService;
import nl.rivm.screenit.model.PostcodeCoordinaten;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.planning.VrijSlot;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LoggingZoekCriteria;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.colon.PlanningService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class IntakeAfpraakServiceImpl implements IntakeAfpraakService
{

	private static final double NORMERINGS_FACTOR = 1.0;

	private static final Logger LOGGER = LoggerFactory.getLogger(IntakeAfpraakServiceImpl.class);

	@Autowired
	private IntakeAfspraakDao intakeAfspraakDao;

	@Autowired
	private PlanningService<VrijSlot> planningService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private InstellingService instellingService;

	@Autowired
	private LogService logService;

	@Override
	public List<ClientAfspraak> getClientenVoorIntakeAfspraakMaken(Integer afstandFactor, Integer tijdFactor, StringBuilder foutmeldingTextUitJobContext)
	{
		Integer intakeafspraakperiode = preferenceService.getInteger(PreferenceKey.INTAKEAFSPRAAKPERIODE.name());
		if (intakeafspraakperiode == null)
		{

			intakeafspraakperiode = Integer.valueOf(14);
		}

		Integer maxAfstandClientColoscopiecentrum = preferenceService.getInteger(PreferenceKey.MAX_AFSTAND_CLIENT_COLOSCOPIECENTRUM.name());
		if (maxAfstandClientColoscopiecentrum == null)
		{

			maxAfstandClientColoscopiecentrum = Integer.valueOf(45);
		}

		double wachttijdNormering = getWachttijdNormering(intakeafspraakperiode, tijdFactor);
		double afstandNormering = getAfstandNormering(maxAfstandClientColoscopiecentrum, afstandFactor);

		Integer uitnodigingsinterval = preferenceService.getInteger(PreferenceKey.UITNODIGINGSINTERVAL.name());
		if (uitnodigingsinterval == null)
		{

			uitnodigingsinterval = Integer.valueOf(732);
		}

		List<Object> rawClienten = intakeAfspraakDao.getClientenVoorIntakeAfspraakMaken(uitnodigingsinterval);
		List<ClientAfspraak> clienten = new ArrayList<>();
		Map<Long, ClientAfspraak> hash = new HashMap<>();
		if (rawClienten != null)
		{
			for (Object rawClientRow : rawClienten)
			{

				Object[] rawClientRowCells = (Object[]) rawClientRow;
				int index = 0;

				Object rawClientId = rawClientRowCells[index++];
				Object rawColonScreeningRondeId = rawClientRowCells[index++];
				Object rawAnalyseDatum = rawClientRowCells[index++];
				Object rawGbaLatitude = rawClientRowCells[index++];
				Object rawGbaLongitude = rawClientRowCells[index++];
				Object rawGemLatitude = rawClientRowCells[index++];
				Object rawGemLongitude = rawClientRowCells[index++];
				Object rawGemNaam = rawClientRowCells[index++];
				Object rawSOId = rawClientRowCells[index++];
				Object intakeAfspraakId = rawClientRowCells[index++];
				Object bsn = rawClientRowCells[index++];
				LOGGER.trace("Client " + bsn + " " + rawClientRowCells[index++]);

				if (rawSOId == null)
				{
					String melding = "Client " + bsn + " is aan gemeente " + rawGemNaam
						+ " gekoppeld. Alleen deze gemeente is niet gekoppeld aan een screeningsorganisatie/regio. Overgeslagen.";
					LoggingZoekCriteria loggingZoekCriteria = new LoggingZoekCriteria();
					loggingZoekCriteria.setMelding(melding);
					List<LogGebeurtenis> gebeurtenissen = new ArrayList<>();
					gebeurtenissen.add(LogGebeurtenis.INTAKE_AFSPRAAK_MAKEN_AFGEROND);
					loggingZoekCriteria.setGebeurtenis(gebeurtenissen);

					if (logService.countLogRegels(loggingZoekCriteria) > 0)
					{
						LOGGER.warn(melding + " Melding wordt geskipped, hebben we al eerder gehad.");
					}
					else
					{
						if (foutmeldingTextUitJobContext.length() > 0)
						{
							if (!foutmeldingTextUitJobContext.toString().contains(melding))
							{
								foutmeldingTextUitJobContext.append("<br>").append(melding);
							}
						}
						else
						{
							foutmeldingTextUitJobContext.append(melding);
						}
						LOGGER.warn(melding);
					}

					continue;
				}

				boolean isAlClientAfspraakDezeBatch = false;

				Long colonScreeningRondeId = (Long) rawColonScreeningRondeId;
				Date analyseDatum = (Date) rawAnalyseDatum;
				if (hash.containsKey(colonScreeningRondeId))
				{
					ClientAfspraak oldAfspraak = hash.get(colonScreeningRondeId);
					isAlClientAfspraakDezeBatch = true;

					if (oldAfspraak.getAnalyseDatum() == null || oldAfspraak.getAnalyseDatum().before(analyseDatum))
					{
						oldAfspraak.setAnalyseDatum(analyseDatum);
					}
				}

				if (!isAlClientAfspraakDezeBatch)
				{
					ClientAfspraak rawAfspraak = new ClientAfspraak();
					rawAfspraak.setClientId((Long) rawClientId);
					rawAfspraak.setColonScreeningRondeId(colonScreeningRondeId);
					rawAfspraak.setAnalyseDatum(analyseDatum);
					rawAfspraak.setIntakeAfspraakId((Long) intakeAfspraakId);
					if (rawGbaLatitude != null)
					{
						rawAfspraak.setLongitude((BigDecimal) rawGbaLongitude);
						rawAfspraak.setLatitude((BigDecimal) rawGbaLatitude);
					}
					else
					{
						rawAfspraak.setLongitude((BigDecimal) rawGemLongitude);
						rawAfspraak.setLatitude((BigDecimal) rawGemLatitude);
					}
					rawAfspraak.setWachttijdNormering(wachttijdNormering);
					rawAfspraak.setAfstandNormering(afstandNormering);
					rawAfspraak.setDefaultAfstand(maxAfstandClientColoscopiecentrum);
					clienten.add(rawAfspraak);
					hash.put(colonScreeningRondeId, rawAfspraak);
				}
			}
		}

		return clienten;
	}

	@Override
	public List<ClientAfspraak> getClientenVoorIntakeAfspraakMaken(StringBuilder foutTekst)
	{
		Integer afstandFactor = preferenceService.getInteger(PreferenceKey.AFSTANDFACTOR.name());
		if (afstandFactor == null)
		{
			afstandFactor = Integer.valueOf(40);
		}

		Integer tijdFactor = preferenceService.getInteger(PreferenceKey.TIJDFACTOR.name());
		if (tijdFactor == null)
		{
			tijdFactor = Integer.valueOf(60);
		}

		return getClientenVoorIntakeAfspraakMaken(afstandFactor, tijdFactor, foutTekst);
	}

	@Override
	public List<VrijSlot> getAllVrijeSlotenIntakeafspraakperiode(int aantalGeselecteerdeClienten, Date begintijd, Date eindtijd, AtomicInteger aantalExtraDagen)
	{
		List<VrijSlot> vrijeSloten = new ArrayList<>();

		List<ColoscopieCentrum> intakeLocaties = instellingService.getActieveIntakelocaties();

		int i = 0;
		LOGGER.info("Aantal geselecteerde clienten " + aantalGeselecteerdeClienten);

		Integer ongunstigeUitslagWachtPeriode = preferenceService.getInteger(PreferenceKey.ONGUNSTIGE_UITSLAG_WACHT_PERIODE.name());
		if (ongunstigeUitslagWachtPeriode == null)
		{

			ongunstigeUitslagWachtPeriode = Integer.valueOf(2); 
		}
		int days = preferenceService.getInteger(PreferenceKey.COLON_MAX_EXTRA_DAGEN_PLANNING_INTAKE.name())
			+ preferenceService.getInteger(PreferenceKey.INTAKEAFSPRAAKPERIODE.name()) + 1;

		DateTime dateTime = new DateTime(begintijd);
		Date laatsteIntakeDatum = DateUtil.minusWerkdagen(dateTime, ongunstigeUitslagWachtPeriode).plusDays(days).withTimeAtStartOfDay().toDate();

		while (vrijeSloten.size() < aantalGeselecteerdeClienten * 2 && eindtijd.before(laatsteIntakeDatum))
		{
			for (ColoscopieCentrum intakelocatie : intakeLocaties)
			{
				for (VrijSlot vrijSlot : planningService.getBeschikbaarheid(begintijd, eindtijd, intakelocatie))
				{
					LOGGER.trace(vrijSlot.toString());
					PostcodeCoordinaten postcodeCoordinaten = intakelocatie.getPostcodeCoordinaten();
					if (postcodeCoordinaten != null)
					{
						vrijSlot.setLatitude(postcodeCoordinaten.getLatitude());
						vrijSlot.setLongitude(postcodeCoordinaten.getLongitude());
					}
					vrijeSloten.add(vrijSlot);
				}
			}
			LOGGER.info("#" + vrijeSloten.size() + " vrije sloten");
			begintijd = eindtijd;
			eindtijd = new DateTime(eindtijd).plusDays(1).withTimeAtStartOfDay().toDate();
			aantalExtraDagen.set(i);
			i++;
		}
		return vrijeSloten;
	}

	@Override
	public double getWachttijdNormering(Integer intakeAfspraakPeriode, Integer tijdfactor)
	{

		return NORMERINGS_FACTOR / (intakeAfspraakPeriode * 24.0) * (tijdfactor * 1.0);
	}

	@Override
	public double getAfstandNormering(Integer maxAfstandClientColonscopiecentrum, Integer afstandfactor)
	{
		return NORMERINGS_FACTOR / (maxAfstandClientColonscopiecentrum * 1.0) * (afstandfactor * 1.0);
	}
}
