package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.model.ClientAfspraak;
import nl.rivm.screenit.batch.service.ColonIntakeAfspraakService;
import nl.rivm.screenit.dto.colon.IntakeAfspraakMakenDto;
import nl.rivm.screenit.model.BagAdres_;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.Gemeente_;
import nl.rivm.screenit.model.PostcodeCoordinaten_;
import nl.rivm.screenit.model.SingleTableHibernateObject_;
import nl.rivm.screenit.model.TablePerClassHibernateObject_;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonDossier_;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonScreeningRonde_;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.IFOBTTest_;
import nl.rivm.screenit.model.colon.dto.VrijSlot;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LoggingZoekCriteria;
import nl.rivm.screenit.repository.algemeen.ClientRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.colon.PlanningService;
import nl.rivm.screenit.specification.algemeen.DossierSpecification;
import nl.rivm.screenit.specification.algemeen.ScreeningRondeSpecification;
import nl.rivm.screenit.specification.colon.ColonFITSpecification;
import nl.rivm.screenit.specification.colon.ColonScreeningRondeSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.isNietOverledenOfAfgevoerd;
import static nl.rivm.screenit.specification.colon.ColonIntakeAfspraakSpecification.heeftGeenGeplandeIntakeAfspraak;

@Slf4j
@Service
public class ColonIntakeAfspraakServiceImpl implements ColonIntakeAfspraakService
{
	private static final double NORMERINGS_FACTOR = 1.0;

	@Autowired
	private PlanningService<VrijSlot> planningService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private InstellingService instellingService;

	@Autowired
	private LogService logService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private ClientRepository clientRepository;

	@Override
	public List<ClientAfspraak> getClientenVoorIntakeAfspraakMaken(Integer afstandFactor, Integer tijdFactor, StringBuilder foutmeldingTextUitJobContext)
	{
		var intakeafspraakPeriode = preferenceService.getInteger(PreferenceKey.INTAKEAFSPRAAKPERIODE.name(), 14);
		var maxAfstandClientColoscopiecentrum = preferenceService.getInteger(PreferenceKey.MAX_AFSTAND_CLIENT_COLOSCOPIECENTRUM.name(), 45);
		var wachttijdNormering = getWachttijdNormering(intakeafspraakPeriode, tijdFactor);
		var afstandNormering = getAfstandNormering(maxAfstandClientColoscopiecentrum, afstandFactor);

		var rawClienten = getClientenVoorIntakeAfspraakMaken();
		var clienten = new ArrayList<ClientAfspraak>();
		var hash = new HashMap<Long, ClientAfspraak>();
		for (var row : rawClienten)
		{
			var technischeLoggingMelding = "Client (id) " + row.getClientId();
			LOG.trace(technischeLoggingMelding);

			if (row.getScreeningOrganisatieId() == null)
			{
				var additioneleMelding = " is aan gemeente " + row.getGemeenteNaam()
					+ " gekoppeld. Alleen deze gemeente is niet gekoppeld aan een screeningsorganisatie/regio. Overgeslagen.";
				var applicatieLoggingMelding = "Client " + row.getBsn() + additioneleMelding;
				var loggingZoekCriteria = new LoggingZoekCriteria();
				loggingZoekCriteria.setMelding(applicatieLoggingMelding);
				var gebeurtenissen = new ArrayList<LogGebeurtenis>();
				gebeurtenissen.add(LogGebeurtenis.INTAKE_AFSPRAAK_MAKEN_AFGEROND);
				loggingZoekCriteria.setGebeurtenis(gebeurtenissen);

				if (logService.countLogRegels(loggingZoekCriteria) > 0)
				{
					LOG.warn("{}{} Melding wordt geskipped, hebben we al eerder gehad.", technischeLoggingMelding, additioneleMelding);
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
					LOG.warn("{}{}", technischeLoggingMelding, additioneleMelding);
				}

				continue;
			}

			boolean isAlClientAfspraakDezeBatch = false;

			var colonScreeningRondeId = row.getScreeningRondeId();
			var analyseDatum = row.getAnalyseDatum();
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
				rawAfspraak.setClientId(row.getClientId());
				rawAfspraak.setColonScreeningRondeId(row.getScreeningRondeId());
				rawAfspraak.setAnalyseDatum(analyseDatum);
				rawAfspraak.setIntakeAfspraakId(row.getIntakeAfspraakId());
				if (row.getAdresLatitude() != null)
				{
					rawAfspraak.setLongitude(row.getAdresLongitude());
					rawAfspraak.setLatitude(row.getAdresLatitude());
				}
				else
				{
					rawAfspraak.setLongitude(row.getGemeenteLongitude());
					rawAfspraak.setLatitude(row.getGemeenteLatitude());
				}
				rawAfspraak.setWachttijdNormering(wachttijdNormering);
				rawAfspraak.setAfstandNormering(afstandNormering);
				rawAfspraak.setDefaultAfstand(maxAfstandClientColoscopiecentrum);
				clienten.add(rawAfspraak);
				hash.put(colonScreeningRondeId, rawAfspraak);
			}
		}

		return clienten;
	}

	private List<IntakeAfspraakMakenDto> getClientenVoorIntakeAfspraakMaken()
	{
		var uitnodigingsinterval = preferenceService.getInteger(PreferenceKey.UITNODIGINGSINTERVAL.name(), 732);
		var uitnodigingsintervalVerlopen = currentDateSupplier.getLocalDate().minusDays(uitnodigingsinterval);

		return clientRepository.findWith(getClientenVoorIntakeAfspraakMakenSpecification(uitnodigingsintervalVerlopen), IntakeAfspraakMakenDto.class,
			q -> q.projections((cb, r) ->
			{
				var dossierJoin = join(r, Client_.colonDossier);
				var screeningRondeJoin = join(dossierJoin, ColonDossier_.laatsteScreeningRonde);
				var testenJoin = join(screeningRondeJoin, ColonScreeningRonde_.ifobtTesten);
				var persoonJoin = join(r, Client_.persoon);
				var adresJoin = join(persoonJoin, GbaPersoon_.gbaAdres);
				var coordinatenJoin = join(adresJoin, BagAdres_.postcodeCoordinaten, JoinType.LEFT);
				var gemeenteJoin = join(adresJoin, BagAdres_.gbaGemeente, JoinType.LEFT);
				var screeningOrganisatieJoin = join(gemeenteJoin, Gemeente_.screeningOrganisatie, JoinType.LEFT);
				var intakeJoin = join(screeningRondeJoin, ColonScreeningRonde_.laatsteAfspraak, JoinType.LEFT);

				return List.of(
					r.get(SingleTableHibernateObject_.id),
					screeningRondeJoin.get(TablePerClassHibernateObject_.id),
					testenJoin.get(IFOBTTest_.analyseDatum),
					coordinatenJoin.get(PostcodeCoordinaten_.latitude),
					coordinatenJoin.get(PostcodeCoordinaten_.longitude),
					gemeenteJoin.get(Gemeente_.latitude),
					gemeenteJoin.get(Gemeente_.longitude),
					gemeenteJoin.get(Gemeente_.naam),
					screeningOrganisatieJoin.get(SingleTableHibernateObject_.id),
					intakeJoin.get(AbstractHibernateObject_.id),
					persoonJoin.get(GbaPersoon_.bsn),
					persoonJoin.get(GbaPersoon_.geboortedatum)
				);
			}).distinct().all());
	}

	private Specification<Client> getClientenVoorIntakeAfspraakMakenSpecification(LocalDate uitnodigingsintervalVerlopen)
	{
		return (r, q, cb) -> isNietOverledenOfAfgevoerd()
			.and(DossierSpecification.heeftStatus(DossierStatus.ACTIEF).with(Client_.colonDossier))
			.and(ScreeningRondeSpecification.isLopend().with(root -> screeningRondeJoin(root)))
			.and(ColonFITSpecification.heeftStatusIn(List.of(IFOBTTestStatus.UITGEVOERD, IFOBTTestStatus.DOETNIETMEE)).with(root -> ifobtJoin(root)))
			.and(ColonFITSpecification.heeftOngunstigeReguliereOfStudieUitslag().with(root -> ifobtJoin(root)))
			.and(ColonScreeningRondeSpecification.heeftGeenBriefVanTypeIn(BriefType.COLON_BRIEVEN_GEEN_INTAKE_NODIG).with(root -> screeningRondeJoin(root)))
			.and(ColonScreeningRondeSpecification.heeftGeenAfsprakenZonderVervolg(uitnodigingsintervalVerlopen)
				.with(root -> screeningRondeJoin(root)))
			.and(heeftGeenGeplandeIntakeAfspraak().with(root -> join(screeningRondeJoin(root), ColonScreeningRonde_.laatsteAfspraak, JoinType.LEFT)))
			.toPredicate(r, q, cb);
	}

	private Join<ColonScreeningRonde, IFOBTTest> ifobtJoin(From<?, ? extends Client> r)
	{
		var screeningRondeJoin = screeningRondeJoin(r);
		return join(screeningRondeJoin, ColonScreeningRonde_.ifobtTesten);
	}

	private Join<ColonDossier, ColonScreeningRonde> screeningRondeJoin(From<?, ? extends Client> r)
	{
		var dossierJoin = join(r, Client_.colonDossier);
		return join(dossierJoin, ColonDossier_.laatsteScreeningRonde);
	}

	@Override
	public List<ClientAfspraak> getClientenVoorIntakeAfspraakMaken(StringBuilder foutTekst)
	{
		var afstandFactor = preferenceService.getInteger(PreferenceKey.AFSTANDFACTOR.name(), 40);
		var tijdFactor = preferenceService.getInteger(PreferenceKey.TIJDFACTOR.name(), 60);
		return getClientenVoorIntakeAfspraakMaken(afstandFactor, tijdFactor, foutTekst);
	}

	@Override
	public List<VrijSlot> getAllVrijeSlotenIntakeafspraakperiode(int aantalGeselecteerdeClienten, LocalDate beginDatum, LocalDate eindDatum, AtomicInteger aantalExtraDagen)
	{
		var vrijeSloten = new ArrayList<VrijSlot>();

		var intakeLocaties = instellingService.getActieveIntakelocaties();

		LOG.info("Aantal geselecteerde clienten {}", aantalGeselecteerdeClienten);

		var ongunstigeUitslagWachtPeriode = preferenceService.getInteger(PreferenceKey.ONGUNSTIGE_UITSLAG_WACHT_PERIODE.name(), 2);
		var days = preferenceService.getInteger(PreferenceKey.COLON_MAX_EXTRA_DAGEN_PLANNING_INTAKE.name())
			+ preferenceService.getInteger(PreferenceKey.INTAKEAFSPRAAKPERIODE.name()) + 1;

		var laatsteIntakeDatum = DateUtil.minusWerkdagen(beginDatum, ongunstigeUitslagWachtPeriode).plusDays(days);

		aantalExtraDagen.set(-1);
		while (vrijeSloten.size() < aantalGeselecteerdeClienten * 2 && eindDatum.isBefore(laatsteIntakeDatum))
		{
			for (var intakelocatie : intakeLocaties)
			{
				for (var vrijSlot : planningService.getBeschikbaarheid(beginDatum, eindDatum, intakelocatie))
				{
					LOG.trace(vrijSlot.toString());
					var postcodeCoordinaten = intakelocatie.getPostcodeCoordinaten();
					if (postcodeCoordinaten != null)
					{
						vrijSlot.setLatitude(postcodeCoordinaten.getLatitude());
						vrijSlot.setLongitude(postcodeCoordinaten.getLongitude());
					}
					vrijeSloten.add(vrijSlot);
				}
			}
			LOG.info("#{} vrije sloten tussen {} en {}", vrijeSloten.size(), beginDatum, eindDatum);
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
