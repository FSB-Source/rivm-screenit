package nl.rivm.screenit.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.function.Function;

import javax.persistence.criteria.From;
import javax.persistence.criteria.JoinType;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonConclusie;
import nl.rivm.screenit.model.colon.ColonConclusie_;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonDossier_;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak_;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonScreeningRonde_;
import nl.rivm.screenit.model.colon.ConclusieTypeFilter;
import nl.rivm.screenit.model.colon.OpenUitnodiging;
import nl.rivm.screenit.model.colon.WerklijstIntakeFilter;
import nl.rivm.screenit.model.colon.enums.ColonAfspraakStatus;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.colon.planning.ColonAfspraakslot;
import nl.rivm.screenit.model.colon.planning.ColonIntakekamer;
import nl.rivm.screenit.model.colon.planning.ColonTijdslot_;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.OpenUitnodigingUitslag;
import nl.rivm.screenit.repository.algemeen.ClientRepository;
import nl.rivm.screenit.repository.colon.ColonAfspraakslotRepository;
import nl.rivm.screenit.repository.colon.ColonIntakeAfspraakRepository;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.colon.ColonBaseAfspraakService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.colon.ColonHuisartsBerichtService;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.algemeen.PersoonSpecification;
import nl.rivm.screenit.specification.colon.ColonConclusieSpecification;
import nl.rivm.screenit.specification.colon.ColonDossierSpecification;
import nl.rivm.screenit.specification.colon.ColonIntakeAfspraakSpecification;
import nl.rivm.screenit.specification.colon.ColonIntakeKamerSpecification;
import nl.rivm.screenit.specification.colon.ColonScreeningRondeSpecification;
import nl.rivm.screenit.specification.colon.ColonTijdslotSpecification;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.ColonScreeningRondeUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.FITTestUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.support.PropertyComparator;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.RangeSpecification.overlapt;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.heeftBsn;
import static nl.rivm.screenit.specification.colon.ColonAfspraakslotSpecification.heeftGeenAfspraak;
import static nl.rivm.screenit.specification.colon.ColonIntakeAfspraakSpecification.heeftAfspraakNa;
import static nl.rivm.screenit.specification.colon.ColonIntakeAfspraakSpecification.heeftAfspraakVoor;
import static nl.rivm.screenit.specification.colon.ColonIntakeAfspraakSpecification.heeftBezwaar;
import static nl.rivm.screenit.specification.colon.ColonIntakeAfspraakSpecification.heeftGeenNieuweAfspraak;
import static nl.rivm.screenit.specification.colon.ColonIntakeAfspraakSpecification.heeftStatus;
import static nl.rivm.screenit.specification.colon.ColonIntakeAfspraakSpecification.heeftStatusIn;
import static nl.rivm.screenit.specification.colon.ColonIntakeAfspraakSpecification.onderdeelVanLaatsteScreeningRonde;
import static nl.rivm.screenit.specification.colon.ColonTijdslotSpecification.heeftVanaf;
import static nl.rivm.screenit.util.StringUtil.propertyChain;
import static org.springframework.data.jpa.domain.Specification.where;

@Slf4j
@Service
public class ColonBaseAfspraakServiceImpl implements ColonBaseAfspraakService
{

	@Autowired
	private ColonIntakeAfspraakRepository afspraakRepository;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private ColonHuisartsBerichtService berichtenService;

	@Autowired
	private ColonDossierBaseService dossierBaseService;

	@Autowired
	private ColonIntakeAfspraakRepository intakeAfspraakRepository;

	@Autowired
	private ColonAfspraakslotRepository afspraakslotRepository;

	@Autowired
	private ClientRepository clientRepository;

	@Override
	@Transactional
	public void annuleerAfspraak(ColonIntakeAfspraak intakeAfspraak, Account account, ColonAfspraakStatus status, boolean communicatieTegenhouden)
	{
		var nu = currentDateSupplier.getLocalDateTime();
		var client = intakeAfspraak.getClient();
		var afzegReden = "";
		afspraakAfzeggen(intakeAfspraak, status, nu, communicatieTegenhouden);

		var screeningRonde = intakeAfspraak.getColonScreeningRonde();

		if (screeningRonde.getOpenUitnodiging() == null)
		{
			if (screeningRonde.getStatus() == ScreeningRondeStatus.LOPEND)
			{
				screeningRonde.setStatus(ScreeningRondeStatus.AFGEROND);
				screeningRonde.setStatusDatum(DateUtil.toUtilDate(nu));
			}
		}
		else
		{
			var uitnodiging = screeningRonde.getOpenUitnodiging();
			uitnodiging.setUitslag(null);
			uitnodiging.setDatum(null);
			uitnodiging.setAfspraak(null);
			hibernateService.saveOrUpdate(uitnodiging);
		}
		hibernateService.saveOrUpdate(intakeAfspraak);
		var format = DateUtil.LOCAL_DATE_TIME_FORMAT;
		var melding = String.format("Intake afspraak van %1$s in %2$s van %3$s afgezegd%4$s", format.format(intakeAfspraak.getVanaf()), intakeAfspraak.getKamer().getNaam(),
			intakeAfspraak.getKamer().getIntakelocatie().getNaam(), afzegReden);
		logService.logGebeurtenis(LogGebeurtenis.AFSPRAAK_AFGEZEGD, account, client, melding, Bevolkingsonderzoek.COLON);

	}

	@Override
	@Transactional
	public void afspraakAfzeggen(ColonIntakeAfspraak afspraak, ColonAfspraakStatus status, LocalDateTime nu, boolean communicatieTegenhouden)
	{
		setAfspraakStatus(afspraak, status);
		afspraak.setAfgezegdOp(nu.plus(100, ChronoUnit.MILLIS));
		hibernateService.saveOrUpdate(afspraak);

		var screeningRonde = afspraak.getColonScreeningRonde();
		dossierBaseService.setDatumVolgendeUitnodiging(screeningRonde.getDossier(), ColonUitnodigingsintervalType.GEANNULEERDE_INTAKE_AFSPRAAK);

		if (!ColonAfspraakStatus.GEANNULEERD_OPEN_UITNODIGING.equals(status))
		{
			var client = afspraak.getClient();
			if (!client.getPersoon().getGbaAdres().getGbaGemeente().getCode().equals(Gemeente.RNI_CODE))
			{
				var colonBrief = briefService.maakBvoBrief(screeningRonde, BriefType.COLON_INTAKE_AFMELDING, DateUtil.toUtilDate(nu.plus(150, ChronoUnit.MILLIS)));
				hibernateService.saveOrUpdate(BriefUtil.setTegenhouden(colonBrief, communicatieTegenhouden));

				if (!communicatieTegenhouden)
				{
					var context = new MailMergeContext();
					context.setClient(client);
					context.setIntakeAfspraak(afspraak);
					if (screeningRonde.getLaatsteUitnodiging() != null)
					{
						context.setColonUitnodiging(screeningRonde.getLaatsteUitnodiging());
					}

					berichtenService.verstuurColonHuisartsBericht(client, screeningRonde, HuisartsBerichtType.ANNULEREN_INTAKEAFSPRAAK, context);
				}

			}
		}
	}

	@Override
	@Transactional
	public void setAfspraakStatus(ColonIntakeAfspraak afspraak, ColonAfspraakStatus status)
	{
		afspraak.setStatus(status);
		if (!ColonAfspraakStatus.GEPLAND.equals(status) && !ColonAfspraakStatus.UITGEVOERD.equals(status))
		{
			var afspraakslot = afspraak.getAfspraakslot();
			if (afspraakslot != null)
			{
				afspraakslot.setAfspraak(null);
				hibernateService.saveOrUpdate(afspraakslot);
				afspraak.setAfspraakslot(null);
			}
		}
	}

	@Override
	public List<ColonIntakeAfspraak> getAfsprakenVoorIntakelocatie(WerklijstIntakeFilter zoekObject, ColonIntakelocatie intakelocatie, long first, long count,
		Sort sort)
	{
		var afspraken = afspraakRepository.findWith(getAfsprakenVoorIntakelocatieSpecification(zoekObject, intakelocatie)
			, q -> q.sortBy(getSorteringVoorAfspraken(sort), (order, r, cb) ->
			{
				var sortProperty = order.getProperty();
				if (sortProperty.startsWith(ColonIntakeAfspraak_.CONCLUSIE))
				{
					join(r, ColonIntakeAfspraak_.conclusie, JoinType.LEFT);
				}
				else if (sortProperty.startsWith(propertyChain(ColonIntakeAfspraak_.CLIENT, Client_.COLON_DOSSIER, ColonDossier_.VOLGENDE_UITNODIGING)))
				{
					var clientJoin = join(r, ColonIntakeAfspraak_.client);
					var dossierJoin = join(clientJoin, Client_.colonDossier);
					join(dossierJoin, ColonDossier_.volgendeUitnodiging);
				}
				return null;
			})).all(first, count);
		if (moetNogOpGeboortedatumFilteren(zoekObject))
		{
			filterGeboortedatum(zoekObject.getGeboortedatum(), afspraken);
		}
		return afspraken;
	}

	private ExtendedSpecification<ColonIntakeAfspraak> getAfsprakenVoorIntakelocatieSpecification(WerklijstIntakeFilter zoekFilter, ColonIntakelocatie intakelocatie)
	{
		var vandaag = currentDateSupplier.getLocalDate();
		var specification = heeftStatusIn(ColonAfspraakStatus.VOOR_AGENDA)
			.and(ColonIntakeKamerSpecification.heeftIntakelocatie(intakelocatie).with(r -> join(r, ColonTijdslot_.kamer)))
			.and(onderdeelVanLaatsteScreeningRonde());

		if (StringUtils.isNotBlank(zoekFilter.getBsn()) && (ColonAfspraakStatus.UITGEVOERD != zoekFilter.getStatus() || zoekFilter.getGeboortedatum() != null))
		{
			specification = specification.and(PersoonSpecification.heeftBsn(zoekFilter.getBsn()).with(persoonJoin()));
		}
		else if (ColonAfspraakStatus.UITGEVOERD == zoekFilter.getStatus())
		{
			specification = specification.and(PersoonSpecification.heeftBsn("nobsn").with(persoonJoin()));
		}

		LocalDate vanaf = DateUtil.toLocalDate(zoekFilter.getVanaf());
		LocalDate totEnMet = null;
		if (zoekFilter.getTotEnMet() != null)
		{
			totEnMet = DateUtil.toLocalDate(zoekFilter.getTotEnMet()).plusDays(1);
		}
		if (ColonAfspraakStatus.GEPLAND == zoekFilter.getStatus())
		{

			specification = specification.and(heeftStatus(ColonAfspraakStatus.GEPLAND));
			if (vanaf == null || !vanaf.isAfter(vandaag))
			{
				vanaf = vandaag;
			}
			if (Boolean.TRUE.equals(zoekFilter.getEersteKeerZoeken()))
			{
				totEnMet = vandaag.plusDays(1);
			}
		}
		else if (ColonAfspraakStatus.UITGEVOERD == zoekFilter.getStatus())
		{
			specification = specification.and(ColonScreeningRondeSpecification.heeftAfgerondeMdlVerslagen().with(screeningRondeJoin())
				.or(heeftStatus(ColonAfspraakStatus.UITGEVOERD)
					.and(ColonConclusieSpecification.heeftType().with(conclusieJoin()))
					.and(ColonConclusieSpecification.heeftNietTypeIn(List.of(ColonConclusieType.DOORVERWIJZEN_NAAR_ANDER_CENTRUM, ColonConclusieType.ON_HOLD))
						.with(conclusieJoin()))
				)
			);
			specification = specification.and(PersoonSpecification.isNietOverleden().with(persoonJoin()));
			specification = specification.and(PersoonSpecification.valtBinnenLeeftijdGrensRestricties(null, zoekFilter.getMaxLeeftijd(), null, vandaag).with(persoonJoin())
				.or(ColonScreeningRondeSpecification.heeftGeenRondeZonderVerslagNaVerlopenOngunstigeUitslag(vandaag.minusDays(zoekFilter.getInterval())).with(screeningRondeJoin()))
				.or(ColonScreeningRondeSpecification.heeftOpenUitnodiging().with(screeningRondeJoin()))
			);
			specification = specification.and(heeftGeenNieuweAfspraak());
		}
		else
		{
			var heeftGeenConclusie = ColonConclusieSpecification.heeftGeenType().with(conclusieJoin());
			if (totEnMet != null && totEnMet.isBefore(vandaag))
			{
				heeftGeenConclusie = heeftGeenConclusie.and(heeftAfspraakVoor(totEnMet.atStartOfDay()));
			}
			else
			{
				heeftGeenConclusie = heeftGeenConclusie.and(heeftAfspraakVoor(vandaag.atStartOfDay()));
			}

			var heeftConclusieOnHold = ColonConclusieSpecification.heeftType(ColonConclusieType.ON_HOLD).with(conclusieJoin());
			var isDoorverwezenOmMedischeRedenen = ColonConclusieSpecification.heeftType(ColonConclusieType.DOORVERWIJZEN_NAAR_ANDER_CENTRUM).with(conclusieJoin())
				.and(ColonConclusieSpecification.isDoorverwijzingBevestigd(false).with(conclusieJoin())
					.and(ColonIntakeAfspraakSpecification.heeftNieuweAfspraak())
				)
				.or(ColonConclusieSpecification.isDoorverwijzingBevestigd(true).with(conclusieJoin())
					.and(ColonIntakeAfspraakSpecification.heeftGeenNieuweAfspraak())
				);

			if (totEnMet != null)
			{
				heeftConclusieOnHold = heeftConclusieOnHold.and(heeftAfspraakVoor(totEnMet.atStartOfDay()));
				isDoorverwezenOmMedischeRedenen = isDoorverwezenOmMedischeRedenen.and(heeftAfspraakVoor(totEnMet.atStartOfDay()));
			}

			specification = specification.and(ColonDossierSpecification.heeftVolgendeUitnodigingNaInterval().with(dossierJoin()));
			specification = specification.and(heeftGeenConclusie.or(heeftConclusieOnHold).or(isDoorverwezenOmMedischeRedenen));
		}

		if (ColonAfspraakStatus.UITGEVOERD != zoekFilter.getStatus())
		{
			specification = specification.and(ColonScreeningRondeSpecification.heeftGeenAfgerondeMdlVerslagen().with(screeningRondeJoin()));
		}

		if (vanaf != null)
		{
			specification = specification.and((heeftAfspraakNa(vanaf.atStartOfDay())));
		}
		if (zoekFilter.getStatus() != null && totEnMet != null)
		{
			specification = specification.and((heeftAfspraakVoor(totEnMet.atStartOfDay())));
		}
		if (zoekFilter.getConclusieTypeFilter() != null)
		{
			if (ConclusieTypeFilter.GEEN_CONCLUSIE == zoekFilter.getConclusieTypeFilter())
			{
				specification = specification.and(ColonConclusieSpecification.heeftGeenType().with(conclusieJoin()));
			}
			else if (!zoekFilter.getConclusieTypeFilter().getConclusieTypes().isEmpty())
			{
				specification = specification.and((ColonConclusieSpecification.heeftTypeIn(zoekFilter.getConclusieTypeFilter().getConclusieTypes()).with(conclusieJoin())));
			}
		}
		return specification;
	}

	private Function<From<?, ? extends ColonIntakeAfspraak>, From<?, ? extends GbaPersoon>> persoonJoin()
	{
		return r ->
		{
			var clientJoin = join(r, ColonIntakeAfspraak_.client);
			return join(clientJoin, Client_.persoon);
		};
	}

	private Function<From<?, ? extends ColonIntakeAfspraak>, From<?, ? extends ColonDossier>> dossierJoin()
	{
		return r ->
		{
			var clientJoin = join(r, ColonIntakeAfspraak_.client);
			return join(clientJoin, Client_.colonDossier);
		};
	}

	private Function<From<?, ? extends ColonIntakeAfspraak>, From<?, ? extends ColonConclusie>> conclusieJoin()
	{
		return r -> join(r, ColonIntakeAfspraak_.conclusie, JoinType.LEFT);
	}

	private Function<From<?, ? extends ColonIntakeAfspraak>, From<?, ? extends ColonScreeningRonde>> screeningRondeJoin()
	{
		return r -> join(r, ColonIntakeAfspraak_.colonScreeningRonde);
	}

	private Sort getSorteringVoorAfspraken(Sort sort)
	{
		var sortering = new ArrayList<Sort.Order>();

		sort.forEach(order ->
		{
			var sortProperty = order.getProperty();
			sortering.add(order);

			if (sortProperty.equals(propertyChain(ColonIntakeAfspraak_.CONCLUSIE, ColonConclusie_.TYPE)))
			{
				sortering.add(new Sort.Order(order.getDirection(), propertyChain(ColonIntakeAfspraak_.CONCLUSIE, ColonConclusie_.ON_HOLD_REDEN)));
			}

			sortering.add(new Sort.Order(order.getDirection(), AbstractHibernateObject_.ID));
		});

		return Sort.by(sortering);
	}

	@Override
	public long countAfsprakenVoorColoscopiecentrum(WerklijstIntakeFilter zoekObject, ColonIntakelocatie intakelocatie)
	{
		var afspraken = afspraakRepository.findAll(getAfsprakenVoorIntakelocatieSpecification(zoekObject, intakelocatie));
		if (moetNogOpGeboortedatumFilteren(zoekObject))
		{
			filterGeboortedatum(zoekObject.getGeboortedatum(), afspraken);
		}
		return afspraken.size();
	}

	private boolean moetNogOpGeboortedatumFilteren(WerklijstIntakeFilter zoekFilter)
	{
		return ColonAfspraakStatus.UITGEVOERD == zoekFilter.getStatus() && zoekFilter.getGeboortedatum() != null;
	}

	private boolean filterGeboortedatum(Date geboortedatum, List<ColonIntakeAfspraak> afspraken)
	{
		return afspraken.removeIf(a -> !DateUtil.isGeboortedatumGelijk(DateUtil.toLocalDate(geboortedatum), a.getClient()));
	}

	@Override
	@Transactional
	public void verplaatsAfspraak(ColonIntakeAfspraak nieuweAfspraak, Account account, BriefType briefType, boolean briefTegenhouden, boolean binnenRooster,
		boolean verwezenMedischeRedenenDoorInfolijn)
	{
		var colonScreeningRonde = nieuweAfspraak.getColonScreeningRonde();

		var laatsteAfspraak = colonScreeningRonde.getLaatsteAfspraak();
		if (nieuweAfspraak == null || nieuweAfspraak.getId() != null || nieuweAfspraak.equals(laatsteAfspraak) || laatsteAfspraak.getStatus()
			.equals(ColonAfspraakStatus.VERPLAATST))
		{

			return;
		}
		colonScreeningRonde.getAfspraken().add(nieuweAfspraak);

		laatsteAfspraak.setNieuweAfspraak(nieuweAfspraak);
		nieuweAfspraak.setOudeAfspraak(laatsteAfspraak);
		setStatus(nieuweAfspraak, account, laatsteAfspraak, verwezenMedischeRedenenDoorInfolijn);

		var client = laatsteAfspraak.getClient();
		client.getAfspraken().add(nieuweAfspraak);

		colonScreeningRonde.setLaatsteAfspraak(nieuweAfspraak);
		ColonAfspraakslot afspraakslot = null;
		if (binnenRooster)
		{
			afspraakslot = getAfspraakslotVoorAfspraak(nieuweAfspraak);
			nieuweAfspraak.setAfspraakslot(afspraakslot);
		}
		hibernateService.saveOrUpdate(nieuweAfspraak);
		hibernateService.saveOrUpdate(laatsteAfspraak);
		hibernateService.saveOrUpdate(colonScreeningRonde);
		hibernateService.saveOrUpdate(client);
		if (afspraakslot != null)
		{
			afspraakslot.setAfspraak(nieuweAfspraak);
			hibernateService.saveOrUpdate(afspraakslot);
		}

		if (briefType == null)
		{
			briefType = BriefType.COLON_INTAKE_GEWIJZIGD;
		}
		var brief = briefService.maakBvoBrief(colonScreeningRonde, briefType);
		brief.setIntakeAfspraak(nieuweAfspraak);
		if (briefTegenhouden)
		{
			hibernateService.saveOrUpdate(BriefUtil.setTegenhouden(brief, true));
		}
		hibernateService.saveOrUpdate(brief);

		var format = DateUtil.LOCAL_DATE_TIME_FORMAT;
		var melding = String.format("Verplaatst van %1$s in %2$s van %3$s naar %4$s in %5$s van %6$s", format.format(laatsteAfspraak.getVanaf()),
			laatsteAfspraak.getKamer().getNaam(), laatsteAfspraak.getKamer().getIntakelocatie().getNaam(), format.format(nieuweAfspraak.getVanaf()),
			nieuweAfspraak.getKamer().getNaam(), nieuweAfspraak.getKamer().getIntakelocatie().getNaam());
		if (!briefType.equals(BriefType.COLON_INTAKE_GEWIJZIGD))
		{
			melding += "; afwijkende brief";
		}
		logService.logGebeurtenis(LogGebeurtenis.AFSPRAAK_VERPLAATST, account, client, melding, Bevolkingsonderzoek.COLON);

		if (!briefTegenhouden)
		{
			verstuurWijzigingsberichtNaarHA(nieuweAfspraak, laatsteAfspraak, client);
		}
	}

	private void setStatus(ColonIntakeAfspraak nieuweAfspraak, Account account, ColonIntakeAfspraak laatsteAfspraak, boolean verwezenMedischeRedenenDoorInfolijn)
	{
		account = (Account) HibernateHelper.deproxy(account);
		var lijktOpEenVerwijzing = laatsteAfspraak.getConclusie() == null
			&& laatsteAfspraak.getVanaf().isBefore(currentDateSupplier.getLocalDateTime())
			&& account instanceof InstellingGebruiker
			&& !laatsteAfspraak.getKamer().getIntakelocatie().equals(nieuweAfspraak.getKamer().getIntakelocatie());
		if (lijktOpEenVerwijzing && verwezenMedischeRedenenDoorInfolijn)
		{
			var conclusie = new ColonConclusie();
			conclusie.setType(ColonConclusieType.DOORVERWIJZEN_NAAR_ANDER_CENTRUM);
			conclusie.setDatum(currentDateSupplier.getDate());
			conclusie.setDoorverwijzingBevestigd(false);
			conclusie.setInstellingGebruiker((InstellingGebruiker) account);
			hibernateService.saveOrUpdate(conclusie);
			laatsteAfspraak.setConclusie(conclusie);
			setAfspraakStatus(laatsteAfspraak, ColonAfspraakStatus.UITGEVOERD);
		}
		else
		{
			setAfspraakStatus(laatsteAfspraak, ColonAfspraakStatus.VERPLAATST);
		}
	}

	protected void verstuurWijzigingsberichtNaarHA(ColonIntakeAfspraak nieuweAfspraak, ColonIntakeAfspraak laatsteAfspraak, Client client)
	{

		var context = new MailMergeContext();
		context.setClient(client);
		context.setIntakeAfspraak(nieuweAfspraak);
		context.setVorigeIntakeAfspraak(laatsteAfspraak);

		if (nieuweAfspraak != null && nieuweAfspraak.getColonScreeningRonde() != null && nieuweAfspraak.getColonScreeningRonde().getLaatsteUitnodiging() != null)
		{
			context.setColonUitnodiging(nieuweAfspraak.getColonScreeningRonde().getLaatsteUitnodiging());
		}
		var berichtType = HuisartsBerichtType.WIJZIGING_INTAKEAFSPRAAK;
		if (laatsteAfspraak == null)
		{
			if (nieuweAfspraak != null && nieuweAfspraak.getColonScreeningRonde() != null && nieuweAfspraak.getColonScreeningRonde().getOpenUitnodiging() != null)
			{
				berichtType = HuisartsBerichtType.INTAKE_NA_OPEN_UITNODIGING;
			}
			else
			{
				berichtType = HuisartsBerichtType.ONGUNSTIGE_UITSLAG;
			}
		}
		var ronde = client.getColonDossier().getLaatsteScreeningRonde();
		try
		{
			berichtenService.verstuurColonHuisartsBericht(client, ronde, berichtType, context);
		}
		catch (Exception e)
		{
			LOG.error("Huisarts Bericht kon niet worden aangemaakt. ", e);
		}
	}

	@Override
	@Transactional
	public void maakNieuweAfspraak(Client client, ColonIntakeAfspraak nieuweAfspraak, boolean briefTegenhouden, boolean binnenRooster,
		BriefType briefType, Account account)
	{
		OpenUitnodiging openUitnodiging;
		var colonDossier = client.getColonDossier();
		var nu = currentDateSupplier.getLocalDateTime();
		var laatsteScreeningRonde = colonDossier.getLaatsteScreeningRonde();
		nieuweAfspraak.setColonScreeningRonde(laatsteScreeningRonde);
		nieuweAfspraak.setClient(client);
		nieuweAfspraak.setGewijzigdOp(nu.plus(100, ChronoUnit.MILLIS));
		ColonAfspraakslot afspraakslot;
		if (binnenRooster)
		{
			afspraakslot = getAfspraakslotVoorAfspraak(nieuweAfspraak);
			nieuweAfspraak.setAfspraakslot(afspraakslot);
		}
		else
		{
			afspraakslot = getVrijAfspraakslotVoorAfspraak(nieuweAfspraak);
			if (afspraakslot != null)
			{
				nieuweAfspraak.setAfspraakslot(afspraakslot);
			}
		}
		hibernateService.saveOrUpdate(nieuweAfspraak);

		var heefAlOpenUitnodigingsBriefGehad = false;
		for (var brief : laatsteScreeningRonde.getBrieven())
		{
			if (BriefType.COLON_BEVESTIGING_INTAKE_AFSRPAAK_NA_OPEN_UITNODIGING.equals(brief.getBriefType()))
			{
				heefAlOpenUitnodigingsBriefGehad = true;
				break;
			}
		}

		openUitnodiging = laatsteScreeningRonde.getOpenUitnodiging();
		if (openUitnodiging != null && openUitnodiging.getUitslag() == null)
		{
			openUitnodiging.setUitslag(OpenUitnodigingUitslag.INTAKE_AFSPRAAK);
			openUitnodiging.setAfspraak(nieuweAfspraak);
			hibernateService.saveOrUpdate(openUitnodiging);
			logService.logGebeurtenis(LogGebeurtenis.OPEN_UITNODIGING_REACTIE, account, client, "Client heeft een nieuwe afspraak gemaakt", Bevolkingsonderzoek.COLON);
		}

		if (afspraakslot != null)
		{
			afspraakslot.setAfspraak(nieuweAfspraak);
			hibernateService.saveOrUpdate(afspraakslot);
		}

		laatsteScreeningRonde.getAfspraken().add(nieuweAfspraak);
		var laatsteAfspraak = laatsteScreeningRonde.getLaatsteAfspraak();
		laatsteScreeningRonde.setLaatsteAfspraak(nieuweAfspraak);
		if (heeftOnafgerondeVerwijzingOmMedischeRedenen(laatsteAfspraak))
		{
			laatsteAfspraak.setNieuweAfspraak(nieuweAfspraak);
			nieuweAfspraak.setOudeAfspraak(laatsteAfspraak);
			hibernateService.saveOrUpdateAll(nieuweAfspraak, laatsteAfspraak);
		}
		client.getAfspraken().add(nieuweAfspraak);
		hibernateService.saveOrUpdate(client);
		ColonBrief brief;
		var creatieDatumColonBrief = DateUtil.toUtilDate(nu.plus(150, ChronoUnit.MILLIS));
		if (openUitnodiging != null && !heefAlOpenUitnodigingsBriefGehad)
		{
			brief = briefService.maakBvoBrief(laatsteScreeningRonde, BriefType.COLON_BEVESTIGING_INTAKE_AFSRPAAK_NA_OPEN_UITNODIGING, creatieDatumColonBrief);
			brief.setIntakeAfspraak(nieuweAfspraak);
		}
		else
		{
			if (briefType != null)
			{
				brief = briefService.maakBvoBrief(laatsteScreeningRonde, briefType, creatieDatumColonBrief);
				brief.setIntakeAfspraak(nieuweAfspraak);
			}
			else
			{
				brief = briefService.maakBvoBrief(laatsteScreeningRonde, BriefType.COLON_INTAKE_GEWIJZIGD, creatieDatumColonBrief);
				brief.setIntakeAfspraak(nieuweAfspraak);
			}
		}
		if (briefTegenhouden)
		{
			hibernateService.saveOrUpdate(BriefUtil.setTegenhouden(brief, true));
		}
		hibernateService.saveOrUpdate(brief);

		laatsteScreeningRonde.setStatus(ScreeningRondeStatus.LOPEND);
		laatsteScreeningRonde.setStatusDatum(DateUtil.toUtilDate(nu.plus(200, ChronoUnit.MILLIS)));
		laatsteScreeningRonde.setAfgerondReden(null);
		hibernateService.saveOrUpdate(laatsteScreeningRonde);

		dossierBaseService.setDatumVolgendeUitnodiging(laatsteScreeningRonde.getDossier(), ColonUitnodigingsintervalType.GEPLANDE_INTAKE_AFSPRAAK);

		verstuurWijzigingsberichtNaarHA(nieuweAfspraak, laatsteAfspraak, client);
	}

	@Override
	public void verzendHuisartsBerichtOpnieuw(Client client, Account account)
	{
		var laatsteScreeningRonde = client.getColonDossier().getLaatsteScreeningRonde();
		var colonIntakeAfspraak = laatsteScreeningRonde.getLaatsteAfspraak();
		var berichtType = HuisartsBerichtType.ONGUNSTIGE_UITSLAG;

		switch (colonIntakeAfspraak.getStatus())
		{
		case GEPLAND:
		case UITGEVOERD:
			if (colonIntakeAfspraak.getConclusie() != null && ColonConclusieType.NO_SHOW.equals(colonIntakeAfspraak.getConclusie().getType()))
			{
				berichtType = HuisartsBerichtType.NO_SHOW_INTAKE;
			}
			break;
		case GEANNULEERD_AFMELDEN:
		case GEANNULEERD_VIA_INFOLIJN:
		case GEANNULEERD_CLIENT:
		case GEANNULEERD_OPEN_UITNODIGING:
		case GEANNULEERD_OVERLIJDEN:
			berichtType = HuisartsBerichtType.ANNULEREN_INTAKEAFSPRAAK;
			break;
		}

		var context = new MailMergeContext();
		context.setClient(client);
		context.setIntakeAfspraak(colonIntakeAfspraak);

		try
		{
			berichtenService.verstuurColonHuisartsBericht(client, laatsteScreeningRonde, laatsteScreeningRonde.getColonHuisarts(), berichtType, context, true);
		}
		catch (Exception e)
		{
			LOG.error("Huisarts Bericht kon niet worden aangemaakt. ", e);
		}
	}

	@Override
	public boolean magWijzigenAfzeggen(ColonIntakeAfspraak afspraak)
	{
		ColonConclusieType colonConclusieType = null;
		var isLaatsteRonde = false;
		var heeftVerslagenInLaatsteRonde = false;
		var rondeAfspraakIsLopend = true;
		var conclusie = afspraak.getConclusie();
		var ronde = afspraak.getColonScreeningRonde();
		isLaatsteRonde = ronde.equals(ronde.getDossier().getLaatsteScreeningRonde());
		if (isLaatsteRonde)
		{
			heeftVerslagenInLaatsteRonde = ColonScreeningRondeUtil.heeftAfgerondeVerslag(ronde);
		}
		if (conclusie != null)
		{
			colonConclusieType = conclusie.getType();
		}
		rondeAfspraakIsLopend = ScreeningRondeStatus.LOPEND.equals(ronde.getStatus());

		var status = afspraak.getStatus();
		return isLaatsteRonde && !heeftVerslagenInLaatsteRonde && rondeAfspraakIsLopend
			&& (ColonAfspraakStatus.GEPLAND.equals(status) || ColonAfspraakStatus.UITGEVOERD.equals(status) && ColonConclusieType.NO_SHOW.equals(colonConclusieType));
	}

	@Override
	public boolean magNieuweAfspraakMaken(Client client)
	{
		var colonDossier = client.getColonDossier();
		boolean isDossierAangemeld = colonDossier.getAangemeld();

		var laatsteScreeningRonde = colonDossier.getLaatsteScreeningRonde();
		var isLaatsteRondeGeldigEnAangemeld = ColonScreeningRondeUtil.isLaatsteScreeningRondGeldigEnAangemeld(laatsteScreeningRonde);

		if (isDossierAangemeld && isLaatsteRondeGeldigEnAangemeld && !ColonScreeningRondeUtil.heeftBuitenDoelgroepBrief(laatsteScreeningRonde)
			&& !ColonScreeningRondeUtil.heeftAfgerondeVerslag(laatsteScreeningRonde))
		{
			var laatsteAfspraak = laatsteScreeningRonde.getLaatsteAfspraak();
			var isErEenOpenUitnodiging = laatsteScreeningRonde.getOpenUitnodiging() != null;
			var isErEenOpenUitnodigingReactie = isErEenOpenUitnodiging && laatsteScreeningRonde.getOpenUitnodiging().getUitslag() != null;
			if (laatsteAfspraak != null)
			{
				var conclusie = laatsteAfspraak.getConclusie();
				var clientWilAndereIntakeLocatie = ColonAfspraakStatus.UITGEVOERD.equals(laatsteAfspraak.getStatus()) && conclusie != null
					&& ColonConclusieType.CLIENT_WIL_ANDERE_INTAKELOKATIE.equals(conclusie.getType());

				var clientWordtDoorverwezenMedischeReden = heeftOnafgerondeVerwijzingOmMedischeRedenen(laatsteAfspraak);

				return clientWilAndereIntakeLocatie
					|| ColonAfspraakStatus.isGeannuleerd(laatsteAfspraak.getStatus()) && !isErEenOpenUitnodigingReactie
					|| clientWordtDoorverwezenMedischeReden;
			}

			else if (!isErEenOpenUitnodigingReactie)
			{
				if (GbaStatus.AFGEVOERD.equals(client.getGbaStatus()))
				{
					return false;
				}
				if (isErEenOpenUitnodiging)
				{
					return true;
				}
				var ifobtTest = laatsteScreeningRonde.getLaatsteIFOBTTest();
				var isIfobtUitslagOngunstig = FITTestUtil.isOngunstig(ifobtTest);

				if (!isIfobtUitslagOngunstig && !FITTestUtil.isGunstig(ifobtTest))
				{

					ifobtTest = null;
					List<ColonScreeningRonde> rondes = new ArrayList<>(colonDossier.getScreeningRondes());
					Collections.sort(rondes, new PropertyComparator<>("id", false, true));
					for (var ronde : rondes)
					{
						if (!ronde.equals(laatsteScreeningRonde))
						{
							Date vroegsteAnalyseDatum = null;
							for (var test : ronde.getIfobtTesten())
							{
								if (FITTestUtil.isOngunstig(test) && (vroegsteAnalyseDatum == null || vroegsteAnalyseDatum.after(test.getAnalyseDatum())))
								{
									ifobtTest = test;
									isIfobtUitslagOngunstig = true;
									vroegsteAnalyseDatum = test.getAnalyseDatum();
								}
							}
						}
					}

					if (isIfobtUitslagOngunstig)
					{
						for (var afspraak : colonDossier.getClient().getAfspraken())
						{
							if (afspraak.getAangemaaktOp() != null && ifobtTest.getAnalyseDatum() != null && afspraak.getAangemaaktOp()
								.isAfter(DateUtil.toLocalDateTime(ifobtTest.getAnalyseDatum())))
							{

								isIfobtUitslagOngunstig = false;
								break;
							}
						}
					}
				}
				return isIfobtUitslagOngunstig && (IFOBTTestStatus.UITGEVOERD.equals(ifobtTest.getStatus()) || IFOBTTestStatus.DOETNIETMEE.equals(ifobtTest.getStatus()));
			}
		}
		return false;
	}

	@Override
	public boolean heeftOnafgerondeVerwijzingOmMedischeRedenen(ColonIntakeAfspraak intakeAfspraak)
	{
		if (intakeAfspraak != null)
		{
			var conclusie = intakeAfspraak.getConclusie();
			return conclusie != null
				&& conclusie.getType() == ColonConclusieType.DOORVERWIJZEN_NAAR_ANDER_CENTRUM
				&& Boolean.TRUE.equals(conclusie.getDoorverwijzingBevestigd())
				&& intakeAfspraak.getNieuweAfspraak() == null;
		}
		return false;
	}

	@Override
	public List<ColonIntakeAfspraak> getAfsprakenKamersInRange(ColonIntakekamer kamer, Range<LocalDateTime> range)
	{
		return intakeAfspraakRepository.findAll(ColonIntakeAfspraakSpecification.heeftKamer(kamer)
				.and(ColonIntakeAfspraakSpecification.heeftStatusIn(List.of(ColonAfspraakStatus.GEPLAND, ColonAfspraakStatus.UITGEVOERD))
					.and(overlapt(range, r -> r.get(ColonTijdslot_.vanaf), r -> r.get(ColonTijdslot_.tot)))),
			Sort.by(Sort.Order.asc(ColonTijdslot_.VANAF)));
	}

	@Override
	public ColonAfspraakslot getAfspraakslotVoorAfspraak(ColonIntakeAfspraak newAfspraak)
	{
		var range = Range.open(newAfspraak.getVanaf(), newAfspraak.getTot());
		return afspraakslotRepository.findFirst(ColonTijdslotSpecification.<ColonAfspraakslot> heeftKamer(newAfspraak.getKamer())
				.and(overlapt(range, r -> r.get(ColonTijdslot_.vanaf), r -> r.get(ColonTijdslot_.tot))),
			Sort.by(Sort.Order.asc(ColonTijdslot_.VANAF))).orElse(null);
	}

	@Override
	public ColonAfspraakslot getVrijAfspraakslotVoorAfspraak(ColonIntakeAfspraak newAfspraak)
	{
		return afspraakslotRepository.findFirst(ColonTijdslotSpecification.<ColonAfspraakslot> heeftKamer(newAfspraak.getKamer())
				.and(heeftVanaf(newAfspraak.getVanaf()))
				.and(heeftGeenAfspraak()),
			Sort.by(Sort.Order.asc(ColonTijdslot_.VANAF))).orElse(null);
	}

	@Override
	public boolean isDoorverwezenOmMedischeRedenenZonderNieuweAfspraak(Client client)
	{
		var laatsteAfspraak = client.getColonDossier().getLaatsteScreeningRonde().getLaatsteAfspraak();
		if (laatsteAfspraak != null)
		{
			return heeftOnafgerondeVerwijzingOmMedischeRedenen(laatsteAfspraak);
		}
		return false;
	}

	@Override
	public boolean isAfspraakVerwezenOmMedischeRedenen(ColonIntakeAfspraak afspraak)
	{
		var isVerwezen = isAfspraakMedischeVerwijzing(afspraak) && afspraak.getNieuweAfspraak() != null;
		var oudeAfspraak = zoekBevestigdeDoorverwijzendeAfspraak(afspraak);
		if (oudeAfspraak != null)
		{
			isVerwezen = true;
		}
		return isVerwezen;
	}

	@Override
	public ColonIntakeAfspraak zoekBevestigdeDoorverwijzendeAfspraak(ColonIntakeAfspraak afspraak)
	{
		var oudeAfspraak = (ColonIntakeAfspraak) HibernateHelper.deproxy(afspraak.getOudeAfspraak());
		while (oudeAfspraak != null)
		{
			if (isAfspraakMedischeVerwijzing(oudeAfspraak))
			{
				return oudeAfspraak;
			}
			oudeAfspraak = (ColonIntakeAfspraak) HibernateHelper.deproxy(oudeAfspraak.getOudeAfspraak());
		}
		return null;
	}

	private boolean isAfspraakMedischeVerwijzing(ColonIntakeAfspraak afspraak)
	{
		var conclusie = afspraak.getConclusie();
		return conclusie != null && ColonConclusieType.DOORVERWIJZEN_NAAR_ANDER_CENTRUM.equals(conclusie.getType()) && Boolean.TRUE.equals(
			conclusie.getDoorverwijzingBevestigd());
	}

	@Override
	public boolean heeftClientIntakeAfspraakMetConclusieBezwaar(String bsn)
	{
		var spec = where(heeftBsn(bsn).with(Client_.persoon)).and(heeftBezwaar().with(r ->
		{
			var dossier = join(r, Client_.colonDossier);
			var laatsteScreeningRonde = join(dossier, ColonDossier_.laatsteScreeningRonde);
			return join(laatsteScreeningRonde, ColonScreeningRonde_.laatsteAfspraak);
		}));

		return clientRepository.exists(spec);
	}
}
