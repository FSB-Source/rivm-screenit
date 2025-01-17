package nl.rivm.screenit.batch.jobs.cervix.selectie.selectiestep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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
import java.time.temporal.ChronoUnit;
import java.time.temporal.TemporalAdjusters;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Expression;
import javax.persistence.criteria.From;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Root;

import nl.rivm.screenit.batch.jobs.cervix.CervixLabPartitioner;
import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.BagAdres_;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.cervix.CervixCytologieVerslag;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixDossier_;
import nl.rivm.screenit.model.cervix.CervixHpvAnalyseresultaten;
import nl.rivm.screenit.model.cervix.CervixHpvBeoordeling;
import nl.rivm.screenit.model.cervix.CervixHpvBeoordeling_;
import nl.rivm.screenit.model.cervix.CervixMonster_;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde_;
import nl.rivm.screenit.model.cervix.CervixUitstel;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje_;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieUitslag;
import nl.rivm.screenit.model.cervix.enums.CervixHpvBeoordelingWaarde;
import nl.rivm.screenit.model.cervix.enums.CervixLeeftijdcategorie;
import nl.rivm.screenit.model.enums.Deelnamemodus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.specification.DateSpecification;
import nl.rivm.screenit.specification.algemeen.DossierSpecification;
import nl.rivm.screenit.specification.algemeen.GemeenteSpecification;
import nl.rivm.screenit.specification.algemeen.PersoonSpecification;
import nl.rivm.screenit.specification.cervix.CervixCytologieVerslagSpecification;
import nl.rivm.screenit.specification.cervix.CervixDossierSpecification;
import nl.rivm.screenit.specification.cervix.CervixHpvAnalyseresultatenSpecification;
import nl.rivm.screenit.specification.cervix.CervixHpvBeoordelingSpecification;
import nl.rivm.screenit.specification.cervix.CervixScreeningRondeSpecification;
import nl.rivm.screenit.specification.cervix.CervixUitstelSpecification;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static javax.persistence.criteria.JoinType.LEFT;
import static nl.rivm.screenit.model.cervix.berichten.CervixHpvResultValue.POS_OTHER_HR_HPV;
import static nl.rivm.screenit.specification.ExtendedSpecification.not;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftActieveClient;

@Component
public class CervixSelectieReader extends BaseSpecificationScrollableResultReader<Client>
{

	private final OrganisatieParameterService organisatieParameterService;

	private final HibernateService hibernateService;

	private final ICurrentDateSupplier dateSupplier;

	public CervixSelectieReader(OrganisatieParameterService organisatieParameterService,
		HibernateService hibernateService, ICurrentDateSupplier dateSupplier)
	{
		this.dateSupplier = dateSupplier;
		super.setFetchSize(50);
		this.organisatieParameterService = organisatieParameterService;
		this.hibernateService = hibernateService;
	}

	@Override
	protected Specification<Client> createSpecification()
	{
		var stepContext = getStepExecutionContext();
		Long bmhkLabId = (Long) stepContext.get(CervixLabPartitioner.KEY_BMHK_LAB);

		var vandaag = dateSupplier.getLocalDate();

		var geboortedatumMinimaal = vandaag.minusYears(CervixLeeftijdcategorie._65.getLeeftijd());
		var geboortedatumMinimaalExtraRonde = vandaag.minusYears(CervixLeeftijdcategorie._70.getLeeftijd());
		var exactDertigJaarGeleden = vandaag.minusYears(CervixLeeftijdcategorie._30.getLeeftijd());
		var exact35JaarGeleden = vandaag.minusYears(CervixLeeftijdcategorie._35.getLeeftijd());

		var heeftPOSOtherHRHPVEnPAP2OfPAP3A1 = CervixHpvAnalyseresultatenSpecification.heeftHpvOhr(POS_OTHER_HR_HPV).with(hpvAnalyseResultatenJoin())
			.and(CervixCytologieVerslagSpecification.heeftGeenCytologieVerslag().with(vervolgonderzoekVerslagJoin()))
			.and(CervixCytologieVerslagSpecification.heeftCytologieUitslag(CervixCytologieUitslag.PAP2).with(cytologieVerslagJoin())
				.or(CervixCytologieVerslagSpecification.heeftCytologieUitslag(CervixCytologieUitslag.PAP3A1).with(cytologieVerslagJoin())));

		var heeftPositieveUitslagMetPAP1 = CervixHpvBeoordelingSpecification.heeftHpvUitslag(CervixHpvBeoordelingWaarde.POSITIEF).with(laatsteHpvBeoordelingJoin())
			.and(CervixCytologieVerslagSpecification.heeftGeenCytologieUitslag().with(cytologieVerslagJoin())
				.or(CervixCytologieVerslagSpecification.heeftCytologieUitslag(CervixCytologieUitslag.PAP1).with(cytologieVerslagJoin())))
			.and(CervixCytologieVerslagSpecification.heeftGeenCytologieUitslag().with(vervolgonderzoekVerslagJoin())
				.or(CervixCytologieVerslagSpecification.heeftCytologieUitslag(CervixCytologieUitslag.PAP1).with(vervolgonderzoekVerslagJoin())));

		var clientSpec = PersoonSpecification.isGeborenVoorOfOp(exactDertigJaarGeleden).with(persoonJoin())
			.and(CervixUitstelSpecification.heeftGeannuleerdDatum().with(uitstelJoin())
				.or(CervixScreeningRondeSpecification.heeftGeenUitstel().with(laatsteScreeningrondeJoin()))
				.or(CervixDossierSpecification.heeftVolgendeRondeVoorOfOp(vandaag).with(cervixDossierJoin())))
			.and((CervixDossierSpecification.heeftGeenVolgendeRondeVanaf().with(cervixDossierJoin())
				.and(CervixDossierSpecification.heeftGeenLaatsteScreeningronde().with(cervixDossierJoin())
					.or(CervixScreeningRondeSpecification.isAangemeld(true).with(laatsteScreeningrondeJoin()))
					.or(PersoonSpecification.isGeborenVoorOfOp(exact35JaarGeleden).with(persoonJoin()))))
				.or(CervixDossierSpecification.heeftVolgendeRondeVoorOfOp(vandaag).with(cervixDossierJoin())))
			.and(PersoonSpecification.isGeborenNa(geboortedatumMinimaal).with(persoonJoin())
				.or(CervixScreeningRondeSpecification.heeftLeeftijdCategorie(CervixLeeftijdcategorie._60).with(laatsteScreeningrondeJoin())
					.and(PersoonSpecification.isGeborenNa(geboortedatumMinimaalExtraRonde).with(persoonJoin()))
					.and(heeftPositieveUitslagMetPAP1
						.or(heeftPOSOtherHRHPVEnPAP2OfPAP3A1))
				));

		return clientSpec
			.and(heeftActieveClient())
			.and(GemeenteSpecification.heeftScreeningOrganisatie().with(gemeenteJoin()))
			.and(GemeenteSpecification.heeftBmhkLaboratorium(bmhkLabId).with(gemeenteJoin()))
			.and(DossierSpecification.heeftStatus(DossierStatus.ACTIEF).with(Client_.cervixDossier))
			.and(DossierSpecification.wachtOpStartProject(false).with(Client_.cervixDossier))
			.and(not(DossierSpecification.<CervixDossier> heeftDeelnamemodus(Deelnamemodus.SELECTIEBLOKKADE)).with(cervixDossierJoin()));
	}

	@Override
	protected Class<?> getResultClass()
	{
		return Object[].class;
	}

	@Override
	protected List<Order> getOrders(Root<Client> r, CriteriaBuilder cb)
	{
		var orders = new ArrayList<Order>();
		if (getMaxAantalClienten() != null)
		{
			var persoonJoin = persoonJoin().apply(r);
			var vandaag = dateSupplier.getLocalDate();
			var laatsteDagVanHuidigJaar = vandaag.with(TemporalAdjusters.lastDayOfYear());
			orders.add(cb.asc(
				getGeboortedag(cb, persoonJoin, vandaag, laatsteDagVanHuidigJaar)
			));
		}
		return orders;
	}

	@Override
	protected boolean isDistinct()
	{
		return false;
	}

	@Override
	protected int getMaxResults()
	{
		var maxAantalClienten = getMaxAantalClienten();
		if (maxAantalClienten != null)
		{
			return maxAantalClienten;
		}
		else
		{
			return -1;
		}
	}

	private static Expression<String> getGeboortedag(CriteriaBuilder cb, From<?, ? extends GbaPersoon> persoonJoin, LocalDate vandaag, LocalDate laatsteDagVanHuidigJaar)
	{
		return cb.function(
			"to_char",
			String.class,
			DateSpecification.intervalInDagen(cb, persoonJoin.get(GbaPersoon_.geboortedatum), vandaag.until(laatsteDagVanHuidigJaar, ChronoUnit.DAYS)),
			cb.literal("DDD")
		);
	}

	private Integer getMaxAantalClienten()
	{
		var stepContext = getStepExecutionContext();
		var bmhkLab = hibernateService.get(BMHKLaboratorium.class, (Long) stepContext.get(CervixLabPartitioner.KEY_BMHK_LAB));
		return organisatieParameterService.getOrganisatieParameter(bmhkLab, OrganisatieParameterKey.CERVIX_MAX_AANTAL_CLIENTEN_SELECTIE);
	}

	private static Function<From<?, ? extends Client>, From<?, ? extends CervixDossier>> cervixDossierJoin()
	{
		return q -> join(q, Client_.cervixDossier);
	}

	private static Function<From<?, ? extends Client>, From<?, ? extends GbaPersoon>> persoonJoin()
	{
		return q -> join(q, Client_.persoon);
	}

	private static Function<From<?, ? extends Client>, From<?, ? extends BagAdres>> adresJoin()
	{
		return q ->
		{
			var persoon = persoonJoin().apply(q);
			return join(persoon, GbaPersoon_.gbaAdres);
		};
	}

	private static Function<From<?, ? extends Client>, From<?, ? extends Gemeente>> gemeenteJoin()
	{
		return q ->
		{
			var adres = adresJoin().apply(q);
			return join(adres, BagAdres_.gbaGemeente);
		};
	}

	private static Function<From<?, ? extends Client>, From<?, ? extends CervixScreeningRonde>> laatsteScreeningrondeJoin()
	{
		return q ->
		{
			var dossier = cervixDossierJoin().apply(q);
			return join(dossier, CervixDossier_.laatsteScreeningRonde, LEFT);
		};
	}

	private static Function<From<?, ? extends Client>, From<?, ? extends CervixUitstel>> uitstelJoin()
	{
		return q ->
		{
			var laatsteScreeningRonde = laatsteScreeningrondeJoin().apply(q);
			return join(laatsteScreeningRonde, CervixScreeningRonde_.uitstel, LEFT);
		};
	}

	private static Function<From<?, ? extends Client>, From<?, ? extends CervixHpvBeoordeling>> laatsteHpvBeoordelingJoin()
	{
		return q ->
		{
			var laatsteScreeningRonde = laatsteScreeningrondeJoin().apply(q);
			var monsterHpvUitslag = join(laatsteScreeningRonde, CervixScreeningRonde_.monsterHpvUitslag, LEFT);
			return join(monsterHpvUitslag, CervixMonster_.laatsteHpvBeoordeling, LEFT);
		};
	}

	private static Function<From<?, ? extends Client>, From<?, ? extends CervixHpvAnalyseresultaten>> hpvAnalyseResultatenJoin()
	{
		return q ->
		{
			var laatsteHpvBeoordeling = laatsteHpvBeoordelingJoin().apply(q);
			return join(laatsteHpvBeoordeling, CervixHpvBeoordeling_.analyseresultaten, LEFT);
		};
	}

	private static Function<From<?, ? extends Client>, From<?, ? extends CervixCytologieVerslag>> cytologieVerslagJoin()
	{
		return q ->
		{
			var laatsteScreeningRonde = laatsteScreeningrondeJoin().apply(q);
			var cytologieUitslag = join(laatsteScreeningRonde, CervixScreeningRonde_.uitstrijkjeCytologieUitslag, LEFT);
			return join(cytologieUitslag, CervixUitstrijkje_.cytologieVerslag, LEFT);
		};
	}

	private static Function<From<?, ? extends Client>, From<?, ? extends CervixCytologieVerslag>> vervolgonderzoekVerslagJoin()
	{
		return q ->
		{
			var laatsteScreeningRonde = laatsteScreeningrondeJoin().apply(q);
			var uitstrijkjeVervolgonderzoekUitslag = join(laatsteScreeningRonde, CervixScreeningRonde_.uitstrijkjeVervolgonderzoekUitslag, LEFT);
			return join(uitstrijkjeVervolgonderzoekUitslag, CervixUitstrijkje_.cytologieVerslag, LEFT);
		};
	}
}
