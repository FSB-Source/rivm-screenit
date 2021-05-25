package nl.rivm.screenit.mamma.planning.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import static nl.rivm.screenit.mamma.planning.model.PlanningConstanten.plannenTotEnMetGeboortedatum;
import static nl.rivm.screenit.mamma.planning.model.PlanningConstanten.plannenVanafGeboortedatum;

import java.math.BigDecimal;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.datasource.DataSourceRouter;
import nl.rivm.screenit.mamma.planning.dao.PlanningReadModelDao;
import nl.rivm.screenit.mamma.planning.index.PlanningBlokIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningBlokkadeIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningClientFactorTypeIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningClientIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningClientZonderPostcodeReeksIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningPostcodeReeksIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningScreeningsEenheidIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningScreeningsOrganisatieIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningStandplaatsIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningStandplaatsPeriodeIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningStandplaatsRondeIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningTehuisIndex;
import nl.rivm.screenit.mamma.planning.model.PlanningBlok;
import nl.rivm.screenit.mamma.planning.model.PlanningBlokkade;
import nl.rivm.screenit.mamma.planning.model.PlanningClient;
import nl.rivm.screenit.mamma.planning.model.PlanningConstanten;
import nl.rivm.screenit.mamma.planning.model.PlanningDag;
import nl.rivm.screenit.mamma.planning.model.PlanningPostcodeReeks;
import nl.rivm.screenit.mamma.planning.model.PlanningPostcodeReeksRegio;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsOrganisatie;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaats;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsPeriode;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsRonde;
import nl.rivm.screenit.mamma.planning.model.PlanningTehuis;
import nl.rivm.screenit.mamma.planning.service.PlanningConceptService;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningDoorrekenenManager;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningWijzigingen;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningWijzigingenRoute;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaBlokkade;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaMammografie;
import nl.rivm.screenit.model.mamma.MammaPostcodeReeks;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.MammaTehuis;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaBlokkadeType;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.model.mamma.enums.MammaFollowUpConclusieStatus;
import nl.rivm.screenit.model.mamma.enums.MammaUitstelReden;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.query.ScreenitRestrictions;
import nl.topicuszorg.hibernate.restrictions.NvlRestrictions;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5Session;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Criteria;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Disjunction;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.hibernate.sql.JoinType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.orm.hibernate5.SessionHolder;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionSynchronizationManager;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class PlanningReadModelDaoImpl extends AbstractAutowiredDao implements PlanningReadModelDao, ApplicationListener<ContextRefreshedEvent>
{
	private static final Logger LOG = LoggerFactory.getLogger(PlanningReadModelDaoImpl.class);

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private PlanningConceptService conceptService;

	@Autowired
	private SessionFactory sessionFactory;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Autowired
	private MammaBaseAfspraakService afspraakService;

	private Map<Long, Set<Long>> teLezenStandplaatsPeriodeSetScreeningsOrganisatieMap = new HashMap<>();

	private Map<Long, Set<Long>> teLezenStandplaatsRondeSetScreeningsOrganisatieMap = new HashMap<>();

	private LocalDate herhalenVanaf;

	private boolean doInit = true;

	@Override
	public void onApplicationEvent(ContextRefreshedEvent contextRefreshedEvent)
	{
		if (doInit)
		{
			LOG.info("doInit");
			OpenHibernate5Session.withCommittedTransaction().run(() -> {
				try
				{
					readDataModel();
				}
				catch (Exception e)
				{
					LOG.error("Fout bij opstarten", e);
				}
				finally
				{
					doInit = false;
				}
			});

		}
	}

	@Override
	public void readDataModel()
	{
		LOG.info("readDataModel");

		DataSourceRouter.useReadWrite();

		PlanningWijzigingen.clear();

		PlanningStandplaatsPeriodeIndex.clear();
		PlanningStandplaatsRondeIndex.clear();
		PlanningScreeningsOrganisatieIndex.clear();
		PlanningScreeningsEenheidIndex.clear();
		PlanningBlokIndex.removeAll();
		PlanningBlokkadeIndex.clear();
		PlanningClientZonderPostcodeReeksIndex.clear();
		PlanningPostcodeReeksIndex.clear();
		PlanningStandplaatsIndex.clear();
		PlanningClientIndex.clear();
		PlanningClientFactorTypeIndex.clear();
		PlanningTehuisIndex.clear();

		Object resource = TransactionSynchronizationManager.getResource(sessionFactory);
		Transaction transaction = null;
		SessionHolder holder = null;
		if (resource instanceof SessionHolder)
		{
			holder = (SessionHolder) resource;
			if (holder.getTransaction() == null)
			{
				transaction = hibernateService.getHibernateSession().beginTransaction();
				holder.setTransaction(transaction);
			}
		}
		getSession().flush();
		if (transaction != null)
		{
			transaction.commit();
			holder.setTransaction(null);
		}

		readScreeningsOrganisaties();
		readConstanten();

		for (PlanningScreeningsOrganisatie screeningsOrganisatie : PlanningScreeningsOrganisatieIndex.getScreeningsOrganisaties())
		{
			readScreeningsEenheden(screeningsOrganisatie);
			readStandplaats(screeningsOrganisatie);
		}

		readStandplaatsRonden(teLezenStandplaatsRondeSetScreeningsOrganisatieMap.values().stream().flatMap(Set::stream).collect(Collectors.toSet()));
		readStandplaatsPerioden(teLezenStandplaatsPeriodeSetScreeningsOrganisatieMap.values().stream().flatMap(Set::stream).collect(Collectors.toSet()));

		readTehuizen();
		readClienten();
		readBlokkades();
		readGebruikteCapaciteit();

		conceptService.herhalen(herhalenVanaf);
		conceptService.opslaan();
	}

	@Override
	public void reset(PlanningScreeningsOrganisatie screeningsOrganisatie)
	{
		LOG.info("reset so: " + screeningsOrganisatie.getId());
		getSession().flush();

		for (PlanningScreeningsEenheid screeningsEenheid : screeningsOrganisatie.getScreeningsEenheidSet())
		{
			for (PlanningStandplaatsPeriode standplaatsPeriode : screeningsEenheid.getStandplaatsPeriodeNavigableSet())
			{
				PlanningStandplaatsPeriodeIndex.remove(standplaatsPeriode);
			}

			for (PlanningDag dag : screeningsEenheid.getDagNavigableMap().values())
			{
				dag.getBlokSet().clear();
				dag.setStandplaatsPeriode(null);
			}

			PlanningBlokIndex.removeAll(screeningsEenheid);
			screeningsEenheid.getBlokSet().clear();
			screeningsEenheid.getStandplaatsPeriodeNavigableSet().clear();
			screeningsEenheid.setHerhalingsWeek(screeningsEenheid.getInitieelHerhalingsWeek());
		}

		for (PlanningStandplaats standplaats : screeningsOrganisatie.getStandplaatsSet())
		{
			standplaats.getStandplaatsRondeNavigableSet().clear();
		}

		screeningsOrganisatie.getScreeningsEenheidSet().forEach(this::readBeschikbareCapaciteit);
		screeningsOrganisatie.restConceptGewijzigdDoor();
		readStandplaatsRonden(teLezenStandplaatsRondeSetScreeningsOrganisatieMap.get(screeningsOrganisatie.getId()));
		readStandplaatsPerioden(teLezenStandplaatsPeriodeSetScreeningsOrganisatieMap.get(screeningsOrganisatie.getId()));

		PlanningDoorrekenenManager.run();

		conceptService.opslaan();
	}

	private void readConstanten()
	{
		LOG.info("readConstanten");

		Criteria crit1 = getSession().createCriteria(MammaScreeningsEenheid.class, "screeningsEenheid");
		crit1.add(Restrictions.eq("screeningsEenheid.actief", true));

		crit1.createAlias("screeningsEenheid.standplaatsPerioden", "standplaatsPeriode");
		crit1.add(Restrictions.ge("standplaatsPeriode.totEnMet", dateSupplier.getDateMidnight()));

		crit1.createAlias("standplaatsPeriode.standplaatsRonde", "standplaatsRonde");
		crit1.createAlias("standplaatsRonde.standplaats", "standplaats");
		crit1.add(Restrictions.eq("standplaats.actief", true));

		crit1.createAlias("standplaatsRonde.standplaatsPerioden", "standplaatsPeriode2");

		crit1.setProjection(Projections.projectionList()
			.add(Projections.groupProperty("standplaatsPeriode2.screeningsEenheid.id")) 
			.add(Projections.min("standplaatsPeriode2.screeningsEenheidVolgNr"))); 

		Criteria crit2 = getSession().createCriteria(MammaStandplaatsPeriode.class, "standplaatsPeriode");
		crit2.createAlias("standplaatsPeriode.standplaatsRonde", "standplaatsRonde");
		crit2.createAlias("standplaatsRonde.standplaats", "standplaats");
		crit2.createAlias("standplaats.regio", "regio");

		Disjunction or = Restrictions.disjunction();
		crit2.add(or);
		List<Object[]> list = crit1.list();
		for (Object[] result : list)
		{
			or.add(Restrictions.and(
				Restrictions.eq("standplaatsPeriode.screeningsEenheid.id", result[0]),
				Restrictions.ge("standplaatsPeriode.screeningsEenheidVolgNr", result[1])));
		}

		crit2.setProjection(Projections.projectionList()
			.add(Projections.property("standplaatsPeriode.vanaf")) 
			.add(Projections.property("standplaatsPeriode.totEnMet")) 
			.add(Projections.property("standplaatsPeriode.id")) 
			.add(Projections.property("standplaatsRonde.id")) 
			.add(Projections.property("regio.wekenVanTevorenUitnodigen")) 
			.add(Projections.property("regio.id"))); 

		teLezenStandplaatsPeriodeSetScreeningsOrganisatieMap.entrySet().stream().forEach(entries -> entries.getValue().clear());
		teLezenStandplaatsRondeSetScreeningsOrganisatieMap.entrySet().stream().forEach(entries -> entries.getValue().clear());
		Date plannenVanaf = null;
		Date plannenTotEnMet = null;

		list = crit2.list();
		for (Object[] result : list)
		{
			Date vanaf = DateUtil.toUtilDate(DateUtil.toLocalDate((Date) result[0]).minusWeeks((Integer) result[4]));
			Date totEnMet = (Date) result[1];
			if (plannenVanaf == null)
			{
				plannenVanaf = vanaf;
				plannenTotEnMet = totEnMet;
			}
			else
			{
				plannenVanaf = plannenVanaf.before(vanaf) ? plannenVanaf : vanaf;
				plannenTotEnMet = plannenTotEnMet.after(totEnMet) ? plannenTotEnMet : totEnMet;
			}
			teLezenStandplaatsPeriodeSetScreeningsOrganisatieMap.get(result[5]).add((Long) result[2]);
			teLezenStandplaatsRondeSetScreeningsOrganisatieMap.get(result[5]).add((Long) result[3]);
		}

		Criteria crit3 = getSession().createCriteria(MammaCapaciteitBlok.class, "capaciteitBlok");
		crit3.setProjection(Projections.max("capaciteitBlok.vanaf"));
		Date geplandTotEnMet = (Date) crit3.uniqueResult();

		if (geplandTotEnMet == null)
		{
			geplandTotEnMet = dateSupplier.getDateMidnight();
		}
		herhalenVanaf = DateUtil.toLocalDate(geplandTotEnMet).plusWeeks(1).with(DayOfWeek.MONDAY);

		if (plannenVanaf == null)
		{
			plannenVanaf = plannenTotEnMet = dateSupplier.getDateMidnight();
		}
		LOG.info("Constanten: GeplandTotEnMet: " + geplandTotEnMet + ", plannenVanaf: " + plannenVanaf + ", plannenTotEnMet: " + plannenTotEnMet);

		int vanafLeeftijd = simplePreferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_LEEFTIJD.name());
		int totEnMetLeeftijd = simplePreferenceService.getInteger(PreferenceKey.MAMMA_MAXIMALE_LEEFTIJD.name());

		PlanningConstanten.set(DateUtil.toLocalDate(plannenVanaf),
			Collections.max(Arrays.asList(DateUtil.toLocalDate(geplandTotEnMet), DateUtil.toLocalDate(plannenTotEnMet).plusMonths(6))), dateSupplier.getLocalDateTime(),
			vanafLeeftijd, totEnMetLeeftijd);
	}

	private void readScreeningsOrganisaties()
	{
		LOG.info("readScreeningsOrganisaties");

		Criteria criteria = getSession().createCriteria(ScreeningOrganisatie.class, "screeningsOrganisatie");
		criteria.add(Restrictions.eq("screeningsOrganisatie.actief", true));

		criteria.setProjection(Projections.projectionList()
			.add(Projections.property("screeningsOrganisatie.id")) 
			.add(Projections.property("screeningsOrganisatie.afspraakDrempelBk")) 
			.add(Projections.property("screeningsOrganisatie.factorEersteOnderzoekBk")) 
			.add(Projections.property("screeningsOrganisatie.factorDubbeleTijdBk")) 
			.add(Projections.property("screeningsOrganisatie.factorMinderValideBk")) 
			.add(Projections.property("screeningsOrganisatie.wekenVanTevorenUitnodigen")) 
			.add(Projections.property("screeningsOrganisatie.vervallenCapaciteitsreserveringDagenBk"))); 

		List<Object[]> screeningOrganisaties = criteria.list();
		teLezenStandplaatsPeriodeSetScreeningsOrganisatieMap.clear();
		teLezenStandplaatsRondeSetScreeningsOrganisatieMap.clear();
		for (Object[] so : screeningOrganisaties)
		{
			Long screeningsOrganisatieId = (Long) so[0];
			PlanningScreeningsOrganisatieIndex.put(new PlanningScreeningsOrganisatie(screeningsOrganisatieId, (Integer) so[1], (BigDecimal) so[2], (BigDecimal) so[3],
				(BigDecimal) so[4], (Integer) so[5], (Integer) so[6]));
			teLezenStandplaatsPeriodeSetScreeningsOrganisatieMap.put(screeningsOrganisatieId, new HashSet<>());
			teLezenStandplaatsRondeSetScreeningsOrganisatieMap.put(screeningsOrganisatieId, new HashSet<>());

		}

		LOG.info(screeningOrganisaties.size() + " so's gelezen");
	}

	private void readScreeningsEenheden(PlanningScreeningsOrganisatie screeningsOrganisatie)
	{
		LOG.info("readScreeningsEenheden so: " + screeningsOrganisatie.getId());

		Criteria crit = getSession().createCriteria(MammaScreeningsEenheid.class, "screeningsEenheid");
		crit.createAlias("screeningsEenheid.beoordelingsEenheid", "beoordelingsEenheid");
		crit.createAlias("beoordelingsEenheid.parent", "centraleEenheid");
		crit.createAlias("screeningsEenheid.standplaatsPerioden", "standplaatsPerioden", JoinType.LEFT_OUTER_JOIN);
		crit.createAlias("screeningsEenheid.mammografen", "mammografen", JoinType.LEFT_OUTER_JOIN);

		crit.add(Restrictions.eq("screeningsEenheid.actief", true));
		crit.add(Restrictions.eq("centraleEenheid.regio.id", screeningsOrganisatie.getId()));

		crit.setProjection(Projections.projectionList()
			.add(Projections.groupProperty("screeningsEenheid.id")) 
			.add(Projections.property("screeningsEenheid.uitgenodigdTotEnMet")) 
			.add(Projections.property("screeningsEenheid.uitnodigenTotEnMet")) 
			.add(Projections.countDistinct("mammografen.id")) 
			.add(Projections.property("screeningsEenheid.interval")) 
			.add(Projections.property("screeningsEenheid.herhalingsWeek")) 
			.add(Projections.max("standplaatsPerioden.screeningsEenheidVolgNr"))); 

		List<Object[]> list = crit.list();
		for (Object[] result : list)
		{
			PlanningScreeningsEenheid screeningsEenheid = new PlanningScreeningsEenheid(
				(Long) result[0],
				DateUtil.toLocalDate((Date) result[1]),
				DateUtil.toLocalDate((Date) result[2]),
				(BigDecimal) result[4],
				DateUtil.toLocalDate((Date) result[5]));
			final Long aantalMammografen = Math.max((Long) result[3], 1); 
			screeningsEenheid.setAantalMammografen(aantalMammografen.intValue());
			screeningsEenheid.setScreeningsOrganisatie(screeningsOrganisatie);

			Integer volgNrOffset = (Integer) result[6];
			if (volgNrOffset != null)
			{
				volgNrOffset++;
				screeningsEenheid.setVolgNrOffset(volgNrOffset);
			}

			screeningsOrganisatie.getScreeningsEenheidSet().add(screeningsEenheid);

			readBeschikbareCapaciteit(screeningsEenheid);

			PlanningScreeningsEenheidIndex.put(screeningsEenheid);
		}

		LOG.info(list.size() + " se's gelezen voor so: " + screeningsOrganisatie.getId());
	}

	private void readBeschikbareCapaciteit(PlanningScreeningsEenheid screeningsEenheid)
	{
		LOG.info("readBeschikbareCapaciteit se: " + screeningsEenheid.getId());

		Criteria crit = getSession().createCriteria(MammaCapaciteitBlok.class, "capaciteitBlok");
		crit.createAlias("capaciteitBlok.screeningsEenheid", "screeningsEenheid");

		crit.add(Restrictions.eq("screeningsEenheid.id", screeningsEenheid.getId()));
		crit.add(Restrictions.ge("capaciteitBlok.vanaf", DateUtil.toUtilDate(PlanningConstanten.prognoseVanafDatum)));

		crit.setProjection(Projections.projectionList()
			.add(Projections.property("capaciteitBlok.id")) 
			.add(Projections.property("capaciteitBlok.vanaf")) 
			.add(Projections.property("capaciteitBlok.tot")) 
			.add(Projections.property("capaciteitBlok.aantalOnderzoeken")) 
			.add(Projections.property("capaciteitBlok.blokType")) 
			.add(Projections.property("capaciteitBlok.opmerkingen")) 
			.add(Projections.property("capaciteitBlok.minderValideAfspraakMogelijk")) 
		);

		List<Object[]> list = crit.list();
		for (Object[] result : list)
		{
			LocalDateTime vanaf = DateUtil.toLocalDateTime((java.util.Date) result[1]);
			PlanningBlok blok = new PlanningBlok((Long) result[0], vanaf.toLocalTime(), DateUtil.toLocalTime((Date) result[2]),
				(Integer) result[3], (MammaCapaciteitBlokType) result[4], (String) result[5], (Boolean) result[6]);
			blok.setScreeningsEenheid(screeningsEenheid);

			screeningsEenheid.getBlokSet().add(blok);
			PlanningDag dag = screeningsEenheid.getDagNavigableMap().get(vanaf.toLocalDate());
			dag.getBlokSet().add(blok);
			blok.setDag(dag);

			PlanningBlokIndex.put(blok);

			PlanningWijzigingenRoute wijzigingenRoute = PlanningWijzigingen.getWijzigingenRoute(screeningsEenheid);
			wijzigingenRoute.getBlokSet().add(blok);
			wijzigingenRoute.getDagSet().add(dag);
			wijzigingenRoute.getWeekSet().add(dag.getWeek());
		}
		LOG.info(list.size() + " blokken gelezen voor se: " + screeningsEenheid.getId());
	}

	private void readStandplaats(PlanningScreeningsOrganisatie screeningsOrganisatie)
	{
		LOG.info("readStandplaats so: " + screeningsOrganisatie.getId());

		Criteria crit = getSession().createCriteria(MammaStandplaats.class, "standplaats");
		crit.createAlias("standplaats.regio", "screeningsOrganisatie");

		crit.add(Restrictions.eq("standplaats.actief", true));
		crit.add(Restrictions.eq("screeningsOrganisatie.id", screeningsOrganisatie.getId()));

		crit.setProjection(Projections.projectionList()
			.add(Projections.property("standplaats.id")));

		List<Long> list = crit.list();
		for (Long result1 : list)
		{
			PlanningStandplaats standplaats = new PlanningStandplaats(result1);
			standplaats.setScreeningsOrganisatie(screeningsOrganisatie);

			screeningsOrganisatie.getStandplaatsSet().add(standplaats);

			PlanningStandplaatsIndex.put(standplaats);

			readPostcodeReeksen(standplaats);

			PlanningWijzigingen.getStandplaatsSet().add(standplaats);
		}

		LOG.info(list.size() + " standplaatsen gelezen voor so: " + screeningsOrganisatie.getId());
	}

	private void readPostcodeReeksen(PlanningStandplaats standplaats)
	{
		LOG.info("readPostcodeReeksen standplaats: " + standplaats.getId());

		Criteria crit = getSession().createCriteria(MammaPostcodeReeks.class, "postcodeReeks");
		crit.createAlias("postcodeReeks.standplaats", "standplaats");

		crit.add(Restrictions.eq("standplaats.id", standplaats.getId()));

		crit.setProjection(Projections.projectionList()
			.add(Projections.property("postcodeReeks.id"))
			.add(Projections.property("postcodeReeks.vanPostcode"))
			.add(Projections.property("postcodeReeks.totPostcode")));

		List<Object[]> list = crit.list();
		for (Object[] result : list)
		{
			PlanningPostcodeReeks postcodeReeks = new PlanningPostcodeReeks((Long) result[0], (String) result[1], (String) result[2]);
			postcodeReeks.setStandplaats(standplaats);

			standplaats.getPostcodeReeksSet().add(postcodeReeks);

			PlanningPostcodeReeksIndex.put(postcodeReeks);
			PlanningWijzigingen.getPostcodeReeksSet().add(postcodeReeks);
		}

		LOG.info(list.size() + " postcodereeksen gelezen voor standplaats: " + standplaats.getId());
	}

	private void readStandplaatsRonden(Set<Long> teLezenStandplaatsRondeIdSet)
	{
		LOG.info("readStandplaatsRonden");

		if (!teLezenStandplaatsRondeIdSet.isEmpty())
		{
			Criteria crit = getSession().createCriteria(MammaStandplaatsRonde.class, "standplaatsRonde");

			crit.add(Restrictions.in("standplaatsRonde.id", teLezenStandplaatsRondeIdSet));

			crit.setProjection(Projections.projectionList()
				.add(Projections.property("standplaatsRonde.id")) 
				.add(Projections.property("standplaatsRonde.interval")) 
				.add(Projections.property("standplaatsRonde.standplaats.id")) 
				.add(Projections.property("standplaatsRonde.afspraakDrempel")) 
				.add(Projections.property("standplaatsRonde.achtervangStandplaats.id")) 
				.add(Projections.property("standplaatsRonde.minderValideUitwijkStandplaats.id")) 
				.add(Projections.property("standplaatsRonde.achtervangToegepast")) 
				.add(Projections.property("standplaatsRonde.minderValideUitnodigenVanaf")) 
				.add(Projections.property("standplaatsRonde.extraMinderValideCapaciteitUitgenodigd")) 
			);

			List<Object[]> list = crit.list();
			for (Object[] result : list)
			{
				PlanningStandplaats achtervangStandplaats = result[4] != null ? PlanningStandplaatsIndex.get((Long) result[4]) : null;
				PlanningStandplaats minderValideUitwijkStandplaats = result[5] != null ? PlanningStandplaatsIndex.get((Long) result[5]) : null;

				PlanningStandplaatsRonde standplaatsRonde = new PlanningStandplaatsRonde((Long) result[0], (Integer) result[3], (BigDecimal) result[1],
					achtervangStandplaats, minderValideUitwijkStandplaats, (Boolean) result[6], DateUtil.toLocalDate((Date) result[7]), (BigDecimal) result[8]);
				standplaatsRonde.setStandplaats(PlanningStandplaatsIndex.get((Long) result[2]));

				PlanningStandplaatsRondeIndex.put(standplaatsRonde);
			}
			int aantaalStandplaatsRonden = list.size();

			crit = getSession().createCriteria(MammaStandplaatsRonde.class, "standplaatsRonde");

			crit.add(Restrictions.in("standplaatsRonde.id", teLezenStandplaatsRondeIdSet));
			crit.createAlias("afspraakcapaciteitBeschikbaarVoor", "afspraakcapaciteitBeschikbaarVoor", JoinType.INNER_JOIN);
			crit.add(Restrictions.isNotEmpty("standplaatsRonde.afspraakcapaciteitBeschikbaarVoor"));

			crit.setProjection(Projections.projectionList()
				.add(Projections.property("standplaatsRonde.id")) 
				.add(Projections.property("afspraakcapaciteitBeschikbaarVoor.id")) 
			);
			list = crit.list();
			for (Object[] result : list)
			{
				PlanningStandplaatsRonde planningStandplaatsRonde = PlanningStandplaatsRondeIndex.get((Long) result[0]);
				planningStandplaatsRonde.getAfspraakcapaciteitBeschikbaarVoor().add(PlanningScreeningsOrganisatieIndex.get((Long) result[1]));
			}
			LOG.info(aantaalStandplaatsRonden + " standplaatsronden gelezen");
		}
	}

	private void readStandplaatsPerioden(Set<Long> teLezenStandplaatsPeriodeIdSet)
	{
		LOG.info("readStandplaatsPeriode");

		if (!teLezenStandplaatsPeriodeIdSet.isEmpty())
		{
			Criteria crit = getSession().createCriteria(MammaStandplaatsPeriode.class, "standplaatsPeriode");

			crit.add(Restrictions.in("standplaatsPeriode.id", teLezenStandplaatsPeriodeIdSet));

			crit.setProjection(Projections.projectionList()
				.add(Projections.property("standplaatsPeriode.id")) 
				.add(Projections.property("standplaatsPeriode.screeningsEenheidVolgNr")) 
				.add(Projections.property("standplaatsPeriode.screeningsEenheid.id")) 
				.add(Projections.property("standplaatsPeriode.standplaatsRonde.id")) 
				.add(Projections.property("standplaatsPeriode.vanaf")) 
				.add(Projections.property("standplaatsPeriode.prognose")) 
				.add(Projections.property("standplaatsPeriode.totEnMet")) 
				.add(Projections.property("standplaatsPeriode.standplaatsRondeVolgNr")) 
			);

			List<Object[]> list = crit.list();
			for (Object[] result : list)
			{
				PlanningStandplaatsPeriode standplaatsPeriode = new PlanningStandplaatsPeriode((Long) result[0], (Integer) result[1], (Integer) result[7],
					DateUtil.toLocalDate((Date) result[4]),
					(Boolean) result[5],
					DateUtil.toLocalDate((Date) result[6]));

				PlanningScreeningsEenheid screeningsEenheid = PlanningScreeningsEenheidIndex.get((Long) result[2]);
				screeningsEenheid.getStandplaatsPeriodeNavigableSet().add(standplaatsPeriode);
				standplaatsPeriode.setScreeningsEenheid(screeningsEenheid);

				int currVolgNrOffset = screeningsEenheid.getVolgNrOffset();
				if (currVolgNrOffset > standplaatsPeriode.getScreeningsEenheidVolgNr())
				{
					currVolgNrOffset = standplaatsPeriode.getScreeningsEenheidVolgNr();
				}
				screeningsEenheid.setVolgNrOffset(currVolgNrOffset);
				PlanningStandplaatsRonde standplaatsRonde = PlanningStandplaatsRondeIndex.get((Long) result[3]);
				standplaatsRonde.getStandplaatsPeriodeNavigableSet().add(standplaatsPeriode);
				standplaatsPeriode.setStandplaatsRonde(standplaatsRonde);

				standplaatsRonde.getStandplaats().getStandplaatsRondeNavigableSet().add(standplaatsRonde);

				PlanningStandplaatsPeriodeIndex.put(standplaatsPeriode);

				PlanningWijzigingen.getWijzigingenRoute(screeningsEenheid).setVanafStandplaatsPeriode(standplaatsPeriode);
			}

			LOG.info(list.size() + " standplaatsperioden gelezen");
		}
	}

	private void readTehuizen()
	{
		LOG.info("readTehuizen");

		Criteria crit = getSession().createCriteria(MammaTehuis.class, "tehuis");
		crit.createAlias("tehuis.standplaats", "standplaats");

		crit.add(Restrictions.eq("tehuis.actief", true));

		crit.setProjection(Projections.projectionList()
			.add(Projections.property("tehuis.id")) 
			.add(Projections.property("tehuis.standplaats.id")) 
			.add(Projections.property("standplaats.regio.id")) 
		);

		List<Object[]> list = new ArrayList<>(crit.list());
		for (Object[] result : list)
		{
			PlanningTehuis tehuis = new PlanningTehuis((Long) result[0], PlanningStandplaatsIndex.get((Long) result[1]));

			PlanningScreeningsOrganisatieIndex.get((Long) result[2]).getTehuisSet().add(tehuis);

			PlanningTehuisIndex.put(tehuis);

			PlanningWijzigingen.getTehuisSet().add(tehuis);
		}

		LOG.info(list.size() + " tehuizen gelezen");
	}

	private void readClienten()
	{
		LOG.info("readClienten");

		Map<Long, Date> vorigeScreeningRondeDatumMap = vorigeScreeningRondeCreatieDatumMap();

		Set<Long> suspectDossierIdSet = suspectDossierIdSet();

		Criteria crit = getSession().createCriteria(Client.class, "client");
		crit.createAlias("client.mammaDossier", "dossier");
		crit.createAlias("dossier.deelnamekans", "deelnamekans");
		crit.createAlias("client.persoon", "persoon");
		crit.createAlias("persoon.gbaAdres", "adres");
		crit.createAlias("persoon.tijdelijkGbaAdres", "tijdelijkGbaAdres", JoinType.LEFT_OUTER_JOIN);
		crit.createAlias("adres.gbaGemeente", "gemeente");
		crit.createAlias("gemeente.screeningOrganisatie", "so");
		crit.createAlias("dossier.laatsteScreeningRonde", "laatsteScreeningRonde", JoinType.LEFT_OUTER_JOIN);
		crit.createAlias("laatsteScreeningRonde.laatsteUitstel", "laatsteUitstel", JoinType.LEFT_OUTER_JOIN, Restrictions.isNull("laatsteUitstel.geannuleerdOp"));
		crit.createAlias("laatsteScreeningRonde.laatsteUitnodiging", "laatsteUitnodiging", JoinType.LEFT_OUTER_JOIN);
		crit.createAlias("laatsteUitnodiging.laatsteAfspraak", "laatsteAfspraak", JoinType.LEFT_OUTER_JOIN);
		crit.createAlias("laatsteAfspraak.standplaatsPeriode", "laatsteAfspraakStandplaatsPeriode", JoinType.LEFT_OUTER_JOIN);
		crit.createAlias("dossier.screeningRondeEvent", "screeningRondeEvent", JoinType.LEFT_OUTER_JOIN);

		crit.add(Restrictions.eq("persoon.geslacht", Geslacht.VROUW));
		crit.add(NvlRestrictions.eq("dossier.status", DossierStatus.ACTIEF, "'" + DossierStatus.ACTIEF.toString() + "'"));
		crit.add(Restrictions.or(Restrictions.isNotNull("adres.postcode"), Restrictions.isNotNull("tijdelijkGbaAdres.postcode")));
		ScreenitRestrictions.addClientBaseRestrictions(crit, "client", "persoon");

		crit.add(Restrictions.or(
			Restrictions.between("persoon.geboortedatum", DateUtil.toUtilDate(plannenVanafGeboortedatum), DateUtil.toUtilDate(plannenTotEnMetGeboortedatum)),
			Restrictions.and(Restrictions.isNotNull("laatsteUitstel.id"), Restrictions.isNull("laatsteUitstel.uitnodiging"))));

		crit.setProjection(Projections.projectionList()
			.add(Projections.property("client.id")) 
			.add(Projections.property("persoon.geboortedatum")) 
			.add(Projections.property("adres.postcode")) 
			.add(Projections.property("tijdelijkGbaAdres.postcode")) 
			.add(Projections.property("so.id")) 
			.add(Projections.property("dossier.id")) 
			.add(Projections.property("dossier.doelgroep")) 
			.add(Projections.property("dossier.tehuis.id")) 
			.add(Projections.property("dossier.eersteMammografieAfgerondStandplaatsRonde.id")) 
			.add(Projections.property("dossier.laatsteMammografieAfgerond")) 
			.add(Projections.property("deelnamekans.deelnamekans")) 
			.add(Projections.property("laatsteScreeningRonde.creatieDatum")) 
			.add(Projections.property("laatsteScreeningRonde.standplaatsRonde.id")) 
			.add(Projections.property("laatsteScreeningRonde.isGeforceerd")) 
			.add(Projections.property("laatsteUitstel.standplaats.id")) 
			.add(Projections.property("laatsteUitstel.streefDatum")) 
			.add(Projections.property("laatsteUitstel.uitstelReden")) 
			.add(Projections.property("laatsteUitstel.uitnodiging.id")) 
			.add(Projections.property("laatsteUitnodiging.standplaatsRonde.id")) 
			.add(Projections.property("laatsteAfspraakStandplaatsPeriode.standplaatsRonde.id")) 
			.add(Projections.property("laatsteAfspraak.afgezegdOp")) 
			.add(Projections.property("screeningRondeEvent.voorgaandeScreeningRondes")) 
			.add(Projections.property("laatsteUitnodiging.creatieDatum")) 
			.add(Projections.property("laatsteAfspraak.status")) 
			.add(Projections.property("laatsteAfspraak.vanaf")) 
		);

		List<Object[]> list = new ArrayList<>(crit.list());
		for (Object[] result : list)
		{
			Long clientId = (Long) result[0];
			LocalDate geboortedatum = DateUtil.toLocalDate((Date) result[1]);
			String gbaAdresPostcode = (String) result[2];
			String tijdelijkGbaAdresPostcode = (String) result[3];
			PlanningScreeningsOrganisatie screeningsOrganisatie = PlanningScreeningsOrganisatieIndex.get((Long) result[4]);
			Long dossierId = (Long) result[5];
			MammaDoelgroep doelgroep = (MammaDoelgroep) result[6];
			Long tehuisId = (Long) result[7];
			Long eersteMammografieAfgerondStandplaatsRondeId = (Long) result[8];
			LocalDate laatsteMammografieAfgerondDatum = DateUtil.toLocalDate((Date) result[9]);
			BigDecimal deelnamekans = (BigDecimal) result[10];
			LocalDate screeningRondeCreatieDatum = DateUtil.toLocalDate((Date) result[11]);
			Long screeningRondeStandplaatsRondeId = (Long) result[12];
			Boolean screeningRondeIsGeforceerd = (Boolean) result[13];
			Long uitstelStandplaatsId = (Long) result[14];
			LocalDate uitstelStreefDatum = DateUtil.toLocalDate((Date) result[15]);
			MammaUitstelReden uitstelReden = (MammaUitstelReden) result[16];
			Long uitstelUitnodigingId = (Long) result[17];
			Long uitnodigingStandplaatsRondeId = (Long) result[18];
			Long afspraakStandplaatsRondeId = (Long) result[19];
			Date afspraakAfgezegdOp = (Date) result[20];
			Integer voorgaandeScreeningRondes = (Integer) result[21];
			LocalDate laatsteUitnodigingDatum = DateUtil.toLocalDate((Date) result[22]);
			MammaAfspraakStatus afspraakStatus = (MammaAfspraakStatus) result[23];
			LocalDateTime afspraakMoment = DateUtil.toLocalDateTime((Date) result[24]);

			String postcode = null; 
			PlanningPostcodeReeks postcodeReeks = null;
			PlanningStandplaats standplaatsVanPostcodeOfTehuis = null;
			PlanningTehuis tehuis = null;
			if (tehuisId == null)
			{
				postcode = tijdelijkGbaAdresPostcode != null ? tijdelijkGbaAdresPostcode : gbaAdresPostcode;
				postcodeReeks = PlanningPostcodeReeksIndex.get(postcode);
				if (postcodeReeks != null)
				{
					standplaatsVanPostcodeOfTehuis = postcodeReeks.getStandplaats();
				}
			}
			else
			{
				tehuis = PlanningTehuisIndex.get(tehuisId);
				standplaatsVanPostcodeOfTehuis = tehuis.getStandplaats();
				standplaatsVanPostcodeOfTehuis.getTehuisSet().add(tehuis);
			}

			PlanningStandplaatsRonde screeningRondeStandplaatsRonde = null;
			PlanningStandplaats uitstelStandplaats = null;
			if (screeningRondeStandplaatsRondeId != null)
			{
				screeningRondeStandplaatsRonde = PlanningStandplaatsRondeIndex.get(screeningRondeStandplaatsRondeId);

				if (uitstelStandplaatsId != null
					&& (uitstelUitnodigingId == null || PlanningStandplaatsRondeIndex.get(uitnodigingStandplaatsRondeId) != null))
				{
					uitstelStandplaats = PlanningStandplaatsIndex.get(uitstelStandplaatsId);
				}
			}

			PlanningStandplaats afspraakStandplaats = null;
			if (afspraakStandplaatsRondeId != null
				&& afspraakAfgezegdOp == null
				&& PlanningStandplaatsRondeIndex.get(afspraakStandplaatsRondeId) != null)
			{
				afspraakStandplaats = PlanningStandplaatsRondeIndex.get(afspraakStandplaatsRondeId).getStandplaats();
			}

			if (uitstelStandplaats != null && afspraakStandplaats != null)
			{
				if (uitstelUitnodigingId == null)
				{
					afspraakStandplaats = null;
				}
				else
				{
					uitstelStandplaats = null;
				}
			}

			PlanningClient client = new PlanningClient(
				clientId,
				geboortedatum,
				postcode,
				eersteMammografieAfgerondStandplaatsRondeId == null || PlanningStandplaatsRondeIndex.get(eersteMammografieAfgerondStandplaatsRondeId) != null,
				doelgroep,
				deelnamekans,
				voorgaandeScreeningRondes != null && voorgaandeScreeningRondes > 0,
				screeningsOrganisatie,
				uitstelStandplaats,
				uitstelStandplaats == null ? null : uitstelStreefDatum,
				uitstelStandplaats == null ? null : uitstelUitnodigingId != null,
				uitstelStandplaats == null ? null : uitstelReden,
				afspraakStandplaats,
				tehuis,
				screeningRondeCreatieDatum,
				laatsteMammografieAfgerondDatum,
				suspectDossierIdSet.contains(dossierId),
				laatsteUitnodigingDatum);

			client.setNoShow(afspraakService.isNoShow(afspraakStatus, afspraakMoment));

			PlanningClientIndex.put(client);
			PlanningWijzigingen.getClientSet().add(client);

			if (postcodeReeks != null)
			{
				PlanningPostcodeReeksRegio postcodeReeksRegio = postcodeReeks.getPostcodeReeksRegio(client);
				postcodeReeksRegio.getClientSet().add(client);
				PlanningWijzigingen.getPostcodeReeksRegioSet().add(postcodeReeksRegio);
			}

			if (standplaatsVanPostcodeOfTehuis != null)
			{
				standplaatsVanPostcodeOfTehuis.getScreeningsOrganisatie().getClientList().add(client);
				PlanningClientFactorTypeIndex.put(client);

				if (!standplaatsVanPostcodeOfTehuis.getStandplaatsRondeNavigableSet().isEmpty())
				{
					PlanningStandplaatsRonde volgendeStandplaatsRonde = standplaatsVanPostcodeOfTehuis.getStandplaatsRondeNavigableSet().first();
					if (volgendeStandplaatsRonde.equals(screeningRondeStandplaatsRonde))
					{

						client.setUitgenodigdHuidigeStandplaatsRonde(true);
						client.setUitgenodigdHuidigeStandplaatsRondeIsGeforceerd(screeningRondeIsGeforceerd);

						if (vorigeScreeningRondeDatumMap.containsKey(dossierId))
						{
							client.setVorigeScreeningRondeCreatieDatum(DateUtil.toLocalDate(vorigeScreeningRondeDatumMap.get(dossierId)));
						}
					}
					else
					{
						client.setVorigeScreeningRondeCreatieDatum(screeningRondeCreatieDatum);
					}
				}
			}
			else
			{
				PlanningClientZonderPostcodeReeksIndex.putClient(client);
			}

			if (tehuis != null)
			{
				tehuis.getClientSet().add(client);
			}

			PlanningStandplaats transportStandplaats = null;
			if (afspraakStandplaats != null)
			{
				afspraakStandplaats.getAfspraakSet().add(client);

				if (!afspraakStandplaats.equals(standplaatsVanPostcodeOfTehuis))
				{
					transportStandplaats = afspraakStandplaats;
				}
			}
			else if (uitstelStandplaats != null)
			{
				uitstelStandplaats.getUitstelSet().add(client);

				if (!uitstelStandplaats.equals(standplaatsVanPostcodeOfTehuis))
				{
					transportStandplaats = uitstelStandplaats;
				}
			}
			if (transportStandplaats == null && screeningRondeStandplaatsRonde != null)
			{
				PlanningStandplaats screeningRondeStandplaats = screeningRondeStandplaatsRonde.getStandplaats();
				if (!screeningRondeStandplaats.equals(standplaatsVanPostcodeOfTehuis))
				{
					transportStandplaats = screeningRondeStandplaats;
					screeningRondeStandplaatsRonde.getScreeningRondeTransportSet().add(client);
				}
			}
			if (transportStandplaats != null)
			{
				if (standplaatsVanPostcodeOfTehuis != null)
				{
					standplaatsVanPostcodeOfTehuis.getTransportVanSet().add(client);
				}
				transportStandplaats.getTransportNaarSet().add(client);
			}
		}

		LOG.info(list.size() + " clienten gelezen");
	}

	private Set<Long> suspectDossierIdSet()
	{
		LOG.info("Start suspectDossierIdSet ");
		DetachedCriteria laatsteAfspraakPerDossierCriteria = DetachedCriteria.forClass(MammaDossier.class);
		laatsteAfspraakPerDossierCriteria.createAlias("screeningRondes", "r");
		laatsteAfspraakPerDossierCriteria.createAlias("r.laatsteUitnodiging", "u");
		laatsteAfspraakPerDossierCriteria.createAlias("u.laatsteAfspraak", "a");
		laatsteAfspraakPerDossierCriteria
			.setProjection(Projections.projectionList().add(Projections.groupProperty("id")).add(Projections.max("a.creatiedatum")));

		Criteria crit = getSession().createCriteria(MammaDossier.class);
		crit.createAlias("screeningRondes", "sr");
		crit.createAlias("sr.laatsteUitnodiging", "ui");
		crit.createAlias("ui.laatsteAfspraak", "af");
		crit.createAlias("af.onderzoek", "on");
		crit.createAlias("on.laatsteBeoordeling", "be");

		crit.add(Subqueries.propertiesIn(new String[] { "id", "af.creatiedatum" }, laatsteAfspraakPerDossierCriteria));
		crit.add(Restrictions.or(
			Restrictions.and(
				Restrictions.eq("be.status", MammaBeoordelingStatus.UITSLAG_ONGUNSTIG),
				Restrictions.or(
					Restrictions.isNull("sr.followUpConclusieStatus"),
					Restrictions.eq("sr.followUpConclusieStatus", MammaFollowUpConclusieStatus.FALSE_NEGATIVE),
					Restrictions.eq("sr.followUpConclusieStatus", MammaFollowUpConclusieStatus.TRUE_POSITIVE))),
			Restrictions.and(
				Restrictions.or(Restrictions.eq("be.status", MammaBeoordelingStatus.UITSLAG_GUNSTIG), Restrictions.eq("be.status", MammaBeoordelingStatus.ONBEOORDEELBAAR)),
				Restrictions.eq("sr.followUpConclusieStatus", MammaFollowUpConclusieStatus.FALSE_NEGATIVE))));

		crit.setProjection(Projections.property("id"));

		Set<Long> dossierIdSet = (Set<Long>) crit.list().stream().collect(Collectors.toSet());
		LOG.info("End suspectDossierIdSet. " + dossierIdSet.size() + " suspect dossiers.");
		return dossierIdSet;
	}

	private void readBlokkades()
	{
		LOG.info("readBlokkades");

		Criteria crit = getSession().createCriteria(MammaBlokkade.class, "blokkade");

		crit.add(Restrictions.eq("blokkade.actief", true));
		crit.add(Restrictions.ge("blokkade.totEnMet", DateUtil.toUtilDate(PlanningConstanten.plannenVanafDatum)));
		crit.add(Restrictions.le("blokkade.vanaf", DateUtil.toUtilDate(PlanningConstanten.plannenTotEnMetDatum)));

		crit.setProjection(Projections.projectionList()
			.add(Projections.property("blokkade.id")) 
			.add(Projections.property("blokkade.type")) 
			.add(Projections.property("blokkade.standplaats.id")) 
			.add(Projections.property("blokkade.screeningsEenheid.id")) 
			.add(Projections.property("blokkade.regio.id")) 
			.add(Projections.property("blokkade.vanaf")) 
			.add(Projections.property("blokkade.totEnMet")) 
		);

		List<Object[]> list = crit.list();
		for (Object[] result : list)
		{
			PlanningStandplaats standplaats = null;
			PlanningScreeningsEenheid screeningsEenheid = null;
			PlanningScreeningsOrganisatie screeningsOrganisatie = null;

			MammaBlokkadeType blokkadeType = (MammaBlokkadeType) result[1];
			switch (blokkadeType)
			{
			case SCREENINGS_ORGANISATIE:
				screeningsOrganisatie = PlanningScreeningsOrganisatieIndex.get((Long) result[4]);
				break;
			case SCREENINGS_EENHEID:
				screeningsEenheid = PlanningScreeningsEenheidIndex.get((Long) result[3]);
				break;
			case STANDPLAATS:
				standplaats = PlanningStandplaatsIndex.get((Long) result[2]);
				break;
			}

			PlanningBlokkade blokkade = new PlanningBlokkade(
				(Long) result[0],
				blokkadeType,
				standplaats,
				screeningsEenheid,
				screeningsOrganisatie,
				DateUtil.toLocalDate((Date) result[5]),
				DateUtil.toLocalDate((Date) result[6]));

			PlanningBlokkadeIndex.put(blokkade);

		}

		LOG.info(list.size() + " blokkades gelezen");
	}

	private void readGebruikteCapaciteit()
	{
		LOG.info("readGebruikteCapaciteit");

		Set<Long> teLezenStandplaatsPeriodeIdSet = teLezenStandplaatsPeriodeSetScreeningsOrganisatieMap.values().stream().flatMap(Set::stream).collect(Collectors.toSet());
		if (!teLezenStandplaatsPeriodeIdSet.isEmpty())
		{
			Criteria crit = getSession().createCriteria(MammaMammografie.class, "mammografie");
			crit.createAlias("mammografie.onderzoek", "onderzoek");
			crit.createAlias("onderzoek.afspraak", "afspraak");
			crit.createAlias("afspraak.standplaatsPeriode", "standplaatsPeriode");
			crit.createAlias("standplaatsPeriode.screeningsEenheid", "screeningsEenheid");
			crit.createAlias("afspraak.uitnodiging", "uitnodiging");
			crit.createAlias("uitnodiging.screeningRonde", "screeningRonde");
			crit.createAlias("screeningRonde.dossier", "dossier");
			crit.createAlias("dossier.client", "client");

			crit.add(Restrictions.lt("afspraak.vanaf", DateUtil.toUtilDate(PlanningConstanten.prognoseVanafDatum)));
			crit.add(Restrictions.in("standplaatsPeriode.id", teLezenStandplaatsPeriodeIdSet));

			crit.add(Restrictions.ge("afspraak.vanaf", DateUtil.toUtilDate(PlanningConstanten.plannenVanafDatum)));

			crit.setProjection(Projections.projectionList()
				.add(Projections.property("client.id")) 
				.add(Projections.property("screeningsEenheid.id")) 
				.add(Projections.property("afspraak.vanaf")) 
			);

			List<Object[]> list = crit.list();
			for (Object[] result : list)
			{
				PlanningClient client = PlanningClientIndex.get((Long) result[0]);
				if (client != null)
				{
					PlanningScreeningsEenheid screeningsEenheid = PlanningScreeningsEenheidIndex.get((Long) result[1]);
					PlanningDag dag = screeningsEenheid.getDagNavigableMap().get(DateUtil.toLocalDate((Date) result[2]));

					dag.getBeschikbaar().add(client.getGebruikteCapaciteit(screeningsEenheid.getScreeningsOrganisatie()), client.getBlokType());
				}
			}
		}
	}

	private Map<Long, Date> vorigeScreeningRondeCreatieDatumMap()
	{
		LOG.info("Start vorigeScreeningRondeCreatieDatumMap");

		Criteria crit = getSession().createCriteria(MammaScreeningRonde.class, "screeningRonde");
		crit.createAlias("screeningRonde.dossier", "dossier");
		crit.add(Restrictions.neProperty("screeningRonde.id", "dossier.laatsteScreeningRonde"));
		crit.setProjection(Projections.projectionList()
			.add(Projections.groupProperty("dossier.id"))
			.add(Projections.max("screeningRonde.creatieDatum")));

		Map<Long, Date> vorigeScreeningRondeCreatieDatumMap = new HashMap<>();
		List<Object[]> list = new ArrayList<>(crit.list());
		for (Object[] result : list)
		{
			vorigeScreeningRondeCreatieDatumMap.put((Long) result[0], (Date) result[1]);
		}

		LOG.info("End vorigeScreeningRondeCreatieDatumMap. " + vorigeScreeningRondeCreatieDatumMap.size() + " vorige screening rondes.");
		return vorigeScreeningRondeCreatieDatumMap;
	}

	@Override
	public void addStandplaatsPeriode(PlanningStandplaatsPeriode standplaatsPeriode)
	{
		Long screeningsOrganisatieId = standplaatsPeriode.getScreeningsEenheid().getScreeningsOrganisatie().getId();
		teLezenStandplaatsPeriodeSetScreeningsOrganisatieMap.get(screeningsOrganisatieId).add(standplaatsPeriode.getId());
		teLezenStandplaatsRondeSetScreeningsOrganisatieMap.get(screeningsOrganisatieId).add(standplaatsPeriode.getStandplaatsRonde().getId());
	}

}
