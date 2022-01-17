package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.dao.IntakeAfspraakDao;
import nl.rivm.screenit.batch.model.ClientAfspraak;
import nl.rivm.screenit.batch.service.IntakeAfpraakService;
import nl.rivm.screenit.model.colon.planning.VrijSlot;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LoggingZoekCriteria;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.colon.PlanningService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

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
		var intakeafspraakperiode = preferenceService.getInteger(PreferenceKey.INTAKEAFSPRAAKPERIODE.name());
		if (intakeafspraakperiode == null)
		{

			intakeafspraakperiode = Integer.valueOf(14);
		}

		var maxAfstandClientColoscopiecentrum = preferenceService.getInteger(PreferenceKey.MAX_AFSTAND_CLIENT_COLOSCOPIECENTRUM.name());
		if (maxAfstandClientColoscopiecentrum == null)
		{

			maxAfstandClientColoscopiecentrum = Integer.valueOf(45);
		}

		var wachttijdNormering = getWachttijdNormering(intakeafspraakperiode, tijdFactor);
		var afstandNormering = getAfstandNormering(maxAfstandClientColoscopiecentrum, afstandFactor);

		var uitnodigingsinterval = preferenceService.getInteger(PreferenceKey.UITNODIGINGSINTERVAL.name());
		if (uitnodigingsinterval == null)
		{

			uitnodigingsinterval = Integer.valueOf(732);
		}

		var rawClienten = intakeAfspraakDao.getClientenVoorIntakeAfspraakMaken(uitnodigingsinterval);
		var clienten = new ArrayList<ClientAfspraak>();
		var hash = new HashMap<Long, ClientAfspraak>();
		if (rawClienten != null)
		{
			for (var rawClientRow : rawClienten)
			{

				var rawClientRowCells = (Object[]) rawClientRow;
				int index = 0;

				var rawClientId = rawClientRowCells[index++];
				var rawColonScreeningRondeId = rawClientRowCells[index++];
				var rawAnalyseDatum = rawClientRowCells[index++];
				var rawGbaLatitude = rawClientRowCells[index++];
				var rawGbaLongitude = rawClientRowCells[index++];
				var rawGemLatitude = rawClientRowCells[index++];
				var rawGemLongitude = rawClientRowCells[index++];
				var rawGemNaam = rawClientRowCells[index++];
				var rawSOId = rawClientRowCells[index++];
				var intakeAfspraakId = rawClientRowCells[index++];
				var bsn = rawClientRowCells[index++];
				var technischeLoggingMelding = "Client (id) " + rawClientId;
				LOGGER.trace(technischeLoggingMelding);

				if (rawSOId == null)
				{
					var additioneleMelding = " is aan gemeente " + rawGemNaam
						+ " gekoppeld. Alleen deze gemeente is niet gekoppeld aan een screeningsorganisatie/regio. Overgeslagen.";
					var applicatieLoggingMelding = "Client " + bsn + additioneleMelding;
					var loggingZoekCriteria = new LoggingZoekCriteria();
					loggingZoekCriteria.setMelding(applicatieLoggingMelding);
					var gebeurtenissen = new ArrayList<LogGebeurtenis>();
					gebeurtenissen.add(LogGebeurtenis.INTAKE_AFSPRAAK_MAKEN_AFGEROND);
					loggingZoekCriteria.setGebeurtenis(gebeurtenissen);

					if (logService.countLogRegels(loggingZoekCriteria) > 0)
					{
						LOGGER.warn(technischeLoggingMelding + additioneleMelding + " Melding wordt geskipped, hebben we al eerder gehad.");
					}
					else
					{
						if (foutmeldingTextUitJobContext.length() > 0)
						{
							if (!foutmeldingTextUitJobContext.toString().contains(applicatieLoggingMelding))
							{
								foutmeldingTextUitJobContext.append("<br>").append(applicatieLoggingMelding);
							}
						}
						else
						{
							foutmeldingTextUitJobContext.append(applicatieLoggingMelding);
						}
						LOGGER.warn(technischeLoggingMelding +  additioneleMelding);
					}

					continue;
				}

				boolean isAlClientAfspraakDezeBatch = false;

				var colonScreeningRondeId = (Long) rawColonScreeningRondeId;
				var analyseDatum = (Date) rawAnalyseDatum;
				if (hash.containsKey(colonScreeningRondeId))
				{
					var oldAfspraak = hash.get(colonScreeningRondeId);
					isAlClientAfspraakDezeBatch = true;

					if (oldAfspraak.getAnalyseDatum() == null || oldAfspraak.getAnalyseDatum().before(analyseDatum))
					{
						oldAfspraak.setAnalyseDatum(analyseDatum);
					}
				}

				if (!isAlClientAfspraakDezeBatch)
				{
					var rawAfspraak = new ClientAfspraak();
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
		var afstandFactor = preferenceService.getInteger(PreferenceKey.AFSTANDFACTOR.name());
		if (afstandFactor == null)
		{
			afstandFactor = Integer.valueOf(40);
		}

		var tijdFactor = preferenceService.getInteger(PreferenceKey.TIJDFACTOR.name());
		if (tijdFactor == null)
		{
			tijdFactor = Integer.valueOf(60);
		}

		return getClientenVoorIntakeAfspraakMaken(afstandFactor, tijdFactor, foutTekst);
	}

	@Override
	public List<VrijSlot> getAllVrijeSlotenIntakeafspraakperiode(int aantalGeselecteerdeClienten, LocalDate beginDatum, LocalDate eindDatum, AtomicInteger aantalExtraDagen)
	{
		var vrijeSloten = new ArrayList<VrijSlot>();

		var intakeLocaties = instellingService.getActieveIntakelocaties();

		LOGGER.info("Aantal geselecteerde clienten " + aantalGeselecteerdeClienten);

		var ongunstigeUitslagWachtPeriode = preferenceService.getInteger(PreferenceKey.ONGUNSTIGE_UITSLAG_WACHT_PERIODE.name());
		if (ongunstigeUitslagWachtPeriode == null)
		{

			ongunstigeUitslagWachtPeriode = Integer.valueOf(2); 
		}
		var days = preferenceService.getInteger(PreferenceKey.COLON_MAX_EXTRA_DAGEN_PLANNING_INTAKE.name())
			+ preferenceService.getInteger(PreferenceKey.INTAKEAFSPRAAKPERIODE.name()) + 1;

		var laatsteIntakeDatum = DateUtil.minusWerkdagen(beginDatum, ongunstigeUitslagWachtPeriode).plusDays(days);

		aantalExtraDagen.set(-1);
		while (vrijeSloten.size() < aantalGeselecteerdeClienten * 2 && eindDatum.isBefore(laatsteIntakeDatum))
		{
			for (var intakelocatie : intakeLocaties)
			{
				for (var vrijSlot : planningService.getBeschikbaarheid(DateUtil.toUtilDate(beginDatum), DateUtil.toUtilDate(eindDatum), intakelocatie))
				{
					LOGGER.trace(vrijSlot.toString());
					var postcodeCoordinaten = intakelocatie.getPostcodeCoordinaten();
					if (postcodeCoordinaten != null)
					{
						vrijSlot.setLatitude(postcodeCoordinaten.getLatitude());
						vrijSlot.setLongitude(postcodeCoordinaten.getLongitude());
					}
					vrijeSloten.add(vrijSlot);
				}
			}
			LOGGER.info("#" + vrijeSloten.size() + " vrije sloten tussen " + beginDatum + " en " + eindDatum);
			beginDatum = eindDatum;
			eindDatum = eindDatum.plusDays(1);
			aantalExtraDagen.incrementAndGet();
		}
		if (aantalExtraDagen.get() < 0)
		{
			aantalExtraDagen.set(0);
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
