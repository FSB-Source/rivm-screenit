package nl.rivm.screenit.dao.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.UitnodigingsGebied;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonGeinterpreteerdeUitslag;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.ColonVerslag;
import nl.rivm.screenit.model.colon.ColoscopieCentrumColonCapaciteitVerdeling;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.OpenUitnodiging;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.query.DateRestrictions;
import nl.rivm.screenit.util.query.DateYearRestrictions;
import nl.rivm.screenit.util.query.ScreenitRestrictions;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.hibernate.Criteria;
import org.hibernate.FetchMode;
import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.hibernate.criterion.Conjunction;
import org.hibernate.criterion.CriteriaQuery;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.LogicalExpression;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Property;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.hibernate.engine.spi.TypedValue;
import org.hibernate.sql.JoinType;

import static nl.rivm.screenit.model.colon.IFOBTType.GOLD;

public abstract class ColonRestrictions
{

	public static Criteria getQueryVooraankondigen(Session session, UitnodigingsGebied uitnodigingsGebied, List<Integer> geboortejaren, boolean count, Integer minimaleLeeftijd,
		Integer maximaleLeeftijd, Long projectGroupId, List<Long> exclusieGroepIds, LocalDate vandaag)
	{
		Criteria crit = getBaseCriteria(session, uitnodigingsGebied, minimaleLeeftijd, maximaleLeeftijd, vandaag);

		crit.createAlias("colonDossier", "dossier", JoinType.LEFT_OUTER_JOIN);
		crit.createAlias("dossier.volgendeUitnodiging", "volgendeUitnodiging", JoinType.LEFT_OUTER_JOIN);
		crit.createAlias("volgendeUitnodiging.interval", "interval", JoinType.LEFT_OUTER_JOIN);

		crit.add(getU1BaseCriteria(vandaag, geboortejaren));

		crit.add(Restrictions.eq("dossier.wachtOpStartProject", false));

		if (projectGroupId != null)
		{
			crit.createAlias("projecten", "projectClient");
			crit.add(Restrictions.eq("projectClient.groep.id", projectGroupId));
			crit.add(Restrictions.eq("projectClient.actief", true));
		}
		else if (CollectionUtils.isNotEmpty(exclusieGroepIds))
		{
			crit.createAlias("projecten", "projectClient", JoinType.LEFT_OUTER_JOIN);

			crit.add(Restrictions.or( 
				Restrictions.isNull("projectClient.id"), 
				Restrictions.eq("projectClient.actief", false), 
				Restrictions.not(Restrictions.in("projectClient.groep.id", exclusieGroepIds))));
		}

		if (!count)
		{
			addDefaultOrder(crit);
		}

		return crit;
	}

	public static Criterion getU1BaseCriteria(LocalDate peildatum, List<Integer> geboortejaren)
	{
		return getU1BaseCriteria(peildatum, geboortejaren, peildatum);
	}

	public static Criterion getU1BaseCriteria(LocalDate peildatum, List<Integer> geboortejaren, LocalDate vandaag)
	{
		Conjunction conjunction = Restrictions.conjunction();
		conjunction.add(
			Restrictions.or(
				Restrictions.isNull("dossier.id"), 
				Restrictions.and( 
					Restrictions.isNull("dossier.laatsteScreeningRonde"), 
					Restrictions.eq("dossier.aangemeld", Boolean.TRUE) 
				) 
			)); 
		conjunction.add(Restrictions.or(Restrictions.isNull("volgendeUitnodiging.id"),
			createReferentieCriteria(peildatum, vandaag)));

		if (!CollectionUtils.isEmpty(geboortejaren))
		{
			conjunction.add(DateYearRestrictions.in("persoon.geboortedatum", geboortejaren.toArray()));
		}
		return conjunction;
	}

	public static Criteria getQueryU2(Session session, UitnodigingsGebied uitnodigingsGebied, Integer minimaleLeeftijd, Integer maximaleLeeftijd, LocalDate vandaag)
	{
		Criteria crit = getCriteriaUitnodigen(session, uitnodigingsGebied, minimaleLeeftijd, maximaleLeeftijd, vandaag);

		crit.createAlias("dossier.volgendeUitnodiging", "volgendeUitnodiging", JoinType.INNER_JOIN);
		crit.createAlias("volgendeUitnodiging.interval", "interval", JoinType.INNER_JOIN);
		crit.createAlias("laatsteScreeningRonde.laatsteAfspraak", "afspraak", JoinType.LEFT_OUTER_JOIN);

		crit.add(getU2BaseCriteria(vandaag));

		crit.add(Restrictions.eq("dossier.wachtOpStartProject", false));

		crit.addOrder(Order.asc("laatsteScreeningRonde.creatieDatum"));

		addDefaultOrder(crit);

		return crit;
	}

	public static Conjunction getU2BaseCriteria(LocalDate peildatum)
	{
		return getU2BaseCriteria(peildatum, peildatum);
	}

	public static Conjunction getU2BaseCriteria(LocalDate peildatum, LocalDate vandaag)
	{
		return Restrictions.and(
			createReferentieCriteria(peildatum, vandaag),
			Restrictions.eq("dossier.aangemeld", Boolean.TRUE),
			Restrictions.or(Restrictions.isNull("afspraak.id"),
				Restrictions.ne("afspraak.status", AfspraakStatus.GEPLAND),
				Restrictions.le("afspraak.startTime", DateUtil.toUtilDate(DateUtil.minusWerkdagen(peildatum, 5)))));
	}

	private static Criterion createReferentieCriteria(LocalDate peildatum, LocalDate vandaag)
	{
		long afwijking = ChronoUnit.DAYS.between(vandaag, peildatum);
		Criterion referentieCriteria = Restrictions.or(
			Restrictions.and(
				Restrictions.isNull("volgendeUitnodiging.projectPeildatum"),
				Restrictions.leProperty("volgendeUitnodiging.peildatum", "interval.berekendeReferentieDatum")),
			Restrictions.and(
				Restrictions.isNotNull("volgendeUitnodiging.projectPeildatum"),
				Restrictions.leProperty("volgendeUitnodiging.projectPeildatum", "interval.berekendeReferentieDatum"))
		);
		if (afwijking != 0)
		{
			referentieCriteria = Restrictions.or(
				Restrictions.and(
					Restrictions.isNull("volgendeUitnodiging.projectPeildatum"),
					new SpecialPropertyExpression("volgendeUitnodiging.peildatum", "<=", "interval.berekendeReferentieDatum", " + interval '" + afwijking + "' day")),
				Restrictions.and(
					Restrictions.isNotNull("volgendeUitnodiging.projectPeildatum"),
					new SpecialPropertyExpression("volgendeUitnodiging.projectPeildatum", "<=", "interval.berekendeReferentieDatum", " + interval '" + afwijking + "' day"))
			);
		}
		return referentieCriteria;
	}

	public static DetachedCriteria critAfsprakenZonderVervolg(Date uitnodigingsIntervalVerlopen)
	{

		DetachedCriteria critAfsprakenZonderVervolg = DetachedCriteria.forClass(ColonScreeningRonde.class);
		critAfsprakenZonderVervolg.createAlias("ifobtTesten", "testen");
		critAfsprakenZonderVervolg.createAlias("laatsteAfspraak", "afspraak");
		critAfsprakenZonderVervolg.createAlias("laatsteAfmelding", "afmelding", JoinType.LEFT_OUTER_JOIN);
		critAfsprakenZonderVervolg.createAlias("afspraak.conclusie", "conclusie", JoinType.LEFT_OUTER_JOIN);
		critAfsprakenZonderVervolg.add(Subqueries.propertiesIn(new String[] { "testen.statusDatum", "id" }, critEersteOngunstigeUitslagUitLaatsteRonde()));
		critAfsprakenZonderVervolg.add(Restrictions.le("testen.statusDatum", uitnodigingsIntervalVerlopen));
		critAfsprakenZonderVervolg.add(Restrictions.or(

			Restrictions.and(Restrictions.in("afspraak.status", AfspraakStatus.GEANNULEERD), Restrictions.isNull("conclusie.type")), 

			Restrictions.and(Restrictions.eq("afmelding.type", AfmeldingType.EENMALIG), Restrictions.isNull("afmelding.heraanmeldDatum"),
				Restrictions.isNull("afmelding.heraanmeldStatus"), Restrictions.isNull("conclusie.type")), 

			Restrictions.eq("conclusie.type", ColonConclusieType.NO_SHOW), 

			Restrictions.eq("conclusie.type", ColonConclusieType.CLIENT_WIL_ANDERE_INTAKELOKATIE) 
		));
		critAfsprakenZonderVervolg.setProjection(Projections.id());
		return critAfsprakenZonderVervolg;
	}

	public static DetachedCriteria critEersteOngunstigeUitslagUitLaatsteRonde()
	{

		DetachedCriteria critEersteOngunstigeUitslagUitLaatsteRonde = DetachedCriteria.forClass(ColonDossier.class);
		critEersteOngunstigeUitslagUitLaatsteRonde
			.setProjection(Projections.projectionList().add(Projections.min("testen.statusDatum")).add(Projections.groupProperty("laatsteScreeningRonde.id")));
		critEersteOngunstigeUitslagUitLaatsteRonde.createAlias("laatsteScreeningRonde", "laatsteScreeningRonde");
		critEersteOngunstigeUitslagUitLaatsteRonde.createAlias("laatsteScreeningRonde.ifobtTesten", "testen");
		critEersteOngunstigeUitslagUitLaatsteRonde.add(critOngunstig("testen"));
		return critEersteOngunstigeUitslagUitLaatsteRonde;
	}

	public static LogicalExpression critRondeZonderVerslagNaVerlopenOngunstigeUitslag(Date maxLengteRonde, String testenAlias, String rondeAlias)
	{
		if (StringUtils.isBlank(testenAlias))
		{
			testenAlias = "testen";
		}
		testenAlias = ScreenitRestrictions.fixAlias(testenAlias);
		if (StringUtils.isBlank(rondeAlias))
		{
			rondeAlias = "";
		}
		rondeAlias = ScreenitRestrictions.fixAlias(rondeAlias);
		return Restrictions.and(
			Subqueries.propertiesIn(new String[] { testenAlias + "statusDatum", rondeAlias + "id" }, critEersteOngunstigeUitslagUitLaatsteRonde()),
			Restrictions.le("testen.statusDatum", maxLengteRonde));
	}

	public static DetachedCriteria critOpenUitnodigingNa2jaar(Date uitnodigingsIntervalVerlopen)
	{
		DetachedCriteria critOpenUitnodigingNa2jaar = DetachedCriteria.forClass(OpenUitnodiging.class);
		critOpenUitnodigingNa2jaar.createAlias("oudeAfspraak", "afspraak");
		critOpenUitnodigingNa2jaar.createAlias("afspraak.colonScreeningRonde", "oudeRonde", JoinType.LEFT_OUTER_JOIN);
		critOpenUitnodigingNa2jaar.createAlias("ronde", "ouRonde");
		critOpenUitnodigingNa2jaar.createAlias("oudeRonde.laatsteAfmelding", "afmelding", JoinType.LEFT_OUTER_JOIN);
		critOpenUitnodigingNa2jaar.createAlias("afspraak.conclusie", "conclusie", JoinType.LEFT_OUTER_JOIN);
		critOpenUitnodigingNa2jaar.createAlias("ouRonde.laatsteAfspraak", "ouAfspraak", JoinType.LEFT_OUTER_JOIN);
		critOpenUitnodigingNa2jaar.createAlias("ouRonde.laatsteAfmelding", "ouAfmelding", JoinType.LEFT_OUTER_JOIN);
		critOpenUitnodigingNa2jaar.createAlias("ouAfspraak.conclusie", "ouConclusie", JoinType.LEFT_OUTER_JOIN);
		critOpenUitnodigingNa2jaar.add(Restrictions.le("ouRonde.creatieDatum", uitnodigingsIntervalVerlopen));
		critOpenUitnodigingNa2jaar.setProjection(Property.forName("ronde"));
		critOpenUitnodigingNa2jaar.add(Restrictions.or(

			Restrictions.and(Restrictions.eq("afspraak.status", AfspraakStatus.GEPLAND),
				new SpecialPropertyExpression("afspraak.startTime", "<", "ouRonde.creatieDatum", " - interval '3' day")), 

			Restrictions.and(Restrictions.in("afspraak.status", AfspraakStatus.GEANNULEERD), Restrictions.isNull("conclusie.type")), 

			Restrictions.and(Restrictions.eq("afmelding.type", AfmeldingType.EENMALIG), Restrictions.isNull("afmelding.heraanmeldDatum"),
				Restrictions.isNull("afmelding.heraanmeldStatus"), Restrictions.isNull("conclusie.type")), 

			Restrictions.eq("conclusie.type", ColonConclusieType.NO_SHOW), 

			Restrictions.eq("conclusie.type", ColonConclusieType.CLIENT_WIL_ANDERE_INTAKELOKATIE) 
		));
		critOpenUitnodigingNa2jaar.add(Restrictions.or(

			Restrictions.isNull("ouRonde.laatsteAfspraak"), 

			Restrictions.and(Restrictions.in("ouAfspraak.status", AfspraakStatus.GEANNULEERD), Restrictions.isNull("ouConclusie.type")), 

			Restrictions.and(Restrictions.eq("ouAfmelding.type", AfmeldingType.EENMALIG), Restrictions.isNull("ouAfmelding.heraanmeldDatum"),
				Restrictions.isNull("ouAfmelding.heraanmeldStatus"), Restrictions.isNull("ouConclusie.type")), 

			Restrictions.eq("ouConclusie.type", ColonConclusieType.NO_SHOW), 

			Restrictions.eq("ouConclusie.type", ColonConclusieType.CLIENT_WIL_ANDERE_INTAKELOKATIE) 
		));
		return critOpenUitnodigingNa2jaar;
	}

	public static Criteria getQueryU3(Session session, LocalDate vandaag)
	{
		Criteria crit = getCriteriaUitnodigen(session, vandaag);

		IFOBTTestStatus ifobtStatus = IFOBTTestStatus.NIETTEBEOORDELEN;

		getIFobtStatusCriteria(crit, ifobtStatus);
		addNogGeenUitslagbriefOntvangenCriteria(crit, "laatsteScreeningRonde");

		crit.addOrder(Order.asc("laatsteScreeningRonde.statusDatum"));
		addDefaultOrder(crit);

		return crit;
	}

	public static Criteria getQueryU4(Session session, LocalDate vandaag)
	{
		Criteria crit = getCriteriaUitnodigen(session, vandaag);

		IFOBTTestStatus ifobtStatus = IFOBTTestStatus.VERLOREN;

		getIFobtStatusCriteria(crit, ifobtStatus);
		addNogGeenUitslagbriefOntvangenCriteria(crit, "laatsteScreeningRonde");

		crit.createAlias("laatsteIFOBTTest.colonUitnodiging", "laatsteIFOBTTestUitnodiging", JoinType.LEFT_OUTER_JOIN);
		crit.createAlias("laatsteIFOBTTestExtra.colonUitnodigingExtra", "laatsteIFOBTTestExtraUitnodiging", JoinType.LEFT_OUTER_JOIN);
		crit.add(Restrictions.isNull("laatsteIFOBTTestUitnodiging.retourzendingReden"));
		crit.add(Restrictions.isNull("laatsteIFOBTTestExtraUitnodiging.retourzendingReden"));

		crit.addOrder(Order.asc("laatsteScreeningRonde.statusDatum"));
		addDefaultOrder(crit);

		return crit;
	}

	public static Criteria getQueryU6(Session session, LocalDate vandaag)
	{
		Criteria crit = getCriteriaUitnodigen(session, vandaag);

		IFOBTTestStatus ifobtStatus = IFOBTTestStatus.VERVALDATUMVERLOPEN;

		getIFobtStatusCriteria(crit, ifobtStatus);
		addNogGeenUitslagbriefOntvangenCriteria(crit, "laatsteScreeningRonde");

		crit.addOrder(Order.asc("laatsteScreeningRonde.statusDatum"));
		addDefaultOrder(crit);

		return crit;
	}

	private static void getIFobtStatusCriteria(Criteria crit, IFOBTTestStatus ifobtStatus)
	{
		crit.add(Restrictions.eq("laatsteScreeningRonde.status", ScreeningRondeStatus.LOPEND));

		crit.createAlias("laatsteScreeningRonde.laatsteIFOBTTest", "laatsteIFOBTTest", JoinType.LEFT_OUTER_JOIN);
		crit.createAlias("laatsteScreeningRonde.laatsteIFOBTTestExtra", "laatsteIFOBTTestExtra", JoinType.LEFT_OUTER_JOIN);
		crit.add(Restrictions.eq("laatsteIFOBTTest.status", ifobtStatus));
		crit.add(Restrictions.or(Restrictions.isNull("laatsteIFOBTTestExtra.status"),
			Restrictions.in("laatsteIFOBTTestExtra.status", Arrays.asList(IFOBTTestStatus.UNMUTABLE_EIND_STATUSSEN))));
	}

	private static Criteria getCriteriaUitnodigen(Session session, LocalDate vandaag)
	{
		Criteria crit = getBaseCriteria(session, null, null, null, vandaag);
		addCriteriaUitnodigen(crit);
		return crit;
	}

	private static Criteria getCriteriaUitnodigen(Session session, UitnodigingsGebied uitnodigingsGebied, Integer minimaleLeeftijd, Integer maximaleLeeftijd, LocalDate vandaag)
	{
		Criteria crit = getBaseCriteria(session, uitnodigingsGebied, minimaleLeeftijd, maximaleLeeftijd, vandaag);
		addCriteriaUitnodigen(crit);
		return crit;
	}

	public static Criteria getBaseCriteria(Session session, UitnodigingsGebied uitnodigingsGebied, Integer minimaleLeeftijd, Integer maximaleLeeftijd, LocalDate peildatum)
	{
		Criteria crit = session.createCriteria(Client.class, "rootClient");
		crit.createAlias("rootClient.persoon", "persoon", JoinType.INNER_JOIN);
		crit.createAlias("persoon.gbaAdres", "adres", JoinType.INNER_JOIN);

		crit.setFetchMode("persoon", FetchMode.JOIN);

		ScreenitRestrictions.addClientBaseRestrictions(crit, "rootClient", "persoon");

		crit.add(ScreenitRestrictions.getLeeftijdsgrensRestrictions(minimaleLeeftijd, maximaleLeeftijd, peildatum));

		addCriteriaUitnodigingsGebied(uitnodigingsGebied, crit);

		return crit;
	}

	private static void addCriteriaUitnodigen(Criteria crit)
	{
		crit.createAlias("rootClient.colonDossier", "dossier", JoinType.INNER_JOIN);
		crit.createAlias("dossier.laatsteScreeningRonde", "laatsteScreeningRonde", JoinType.INNER_JOIN);
		crit.createAlias("laatsteScreeningRonde.laatsteUitnodiging", "laatsteUitnodiging", JoinType.LEFT_OUTER_JOIN);
		crit.setFetchMode("rootClient.dossier", FetchMode.JOIN);

		crit.add(Restrictions.or(Restrictions.isNull("laatsteUitnodiging.id"), Restrictions.eq("laatsteUitnodiging.verstuurd", true)));
	}

	private static void addDefaultOrder(Criteria crit)
	{
		crit.addOrder(Order.asc("persoon.geboortedatum"));
		crit.addOrder(Order.asc("persoon.achternaam"));
	}

	public static void addCriteriaUitnodigingsGebied(UitnodigingsGebied uitnodigingsGebied, Criteria crit)
	{
		if (uitnodigingsGebied != null)
		{
			crit.add(Restrictions.eq("adres.gbaGemeente", uitnodigingsGebied.getGemeente()));
			if (uitnodigingsGebied.getPostcodeGebied() != null || uitnodigingsGebied.getGemeenteDeel() != null || uitnodigingsGebied.getWoonplaats() != null)
			{
				crit.add(getCriteriaSplittedUitnodigingsGebied(uitnodigingsGebied, false));
			}
			else if (uitnodigingsGebied.getGemeente().getUitnodigingsGebieden().size() > 1)
			{

				List<UitnodigingsGebied> uitnodigingsGebieden = new ArrayList<>(uitnodigingsGebied.getGemeente().getUitnodigingsGebieden());
				uitnodigingsGebieden.remove(uitnodigingsGebied);

				Conjunction excludes = Restrictions.conjunction();
				for (UitnodigingsGebied uitnodigingsGebiedToExclude : uitnodigingsGebieden)
				{
					Criterion exclusieCriteria = getCriteriaSplittedUitnodigingsGebied(uitnodigingsGebiedToExclude, true);
					if (exclusieCriteria != null)
					{
						excludes.add(exclusieCriteria);
					}
				}

				String field = null;
				if (uitnodigingsGebieden.get(0).getPostcodeGebied() != null)
				{
					field = "adres.postcode";
				}
				else if (uitnodigingsGebieden.get(0).getGemeenteDeel() != null)
				{
					field = "adres.gemeentedeel";
				}
				else if (uitnodigingsGebieden.get(0).getWoonplaats() != null)
				{
					field = "adres.plaats";
				}

				if (field != null)
				{
					crit.add(Restrictions.or(Restrictions.isNull(field), excludes));
				}
			}
		}
	}

	private static Criterion getCriteriaSplittedUitnodigingsGebied(UitnodigingsGebied uitnodigingsGebied, boolean exclude)
	{
		if (uitnodigingsGebied.getPostcodeGebied() != null)
		{
			return getCriteriaPostcode(uitnodigingsGebied, exclude);
		}
		else if (uitnodigingsGebied.getGemeenteDeel() != null)
		{
			if (exclude)
			{
				return Restrictions.ne("adres.gemeentedeel", uitnodigingsGebied.getGemeenteDeel());
			}
			else
			{
				return Restrictions.eq("adres.gemeentedeel", uitnodigingsGebied.getGemeenteDeel());
			}
		}
		else if (uitnodigingsGebied.getWoonplaats() != null)
		{
			if (exclude)
			{
				return Restrictions.ne("adres.plaats", uitnodigingsGebied.getWoonplaats());
			}
			else
			{
				return Restrictions.eq("adres.plaats", uitnodigingsGebied.getWoonplaats());
			}
		}

		return null;
	}

	private static Criterion getCriteriaPostcode(UitnodigingsGebied uitnodigingsGebied, boolean exclude)
	{
		Criterion postcode = Restrictions.and(Restrictions.le("adres.postcode", uitnodigingsGebied.getPostcodeGebied().getTotPostcode().toUpperCase()),
			Restrictions.ge("adres.postcode", uitnodigingsGebied.getPostcodeGebied().getVanPostcode().toUpperCase()));

		if (exclude)
		{
			postcode = Restrictions.not(postcode);
		}

		return postcode;
	}

	public static String getUniekIdOf(ColoscopieCentrumColonCapaciteitVerdeling verdeling)
	{
		if (verdeling.getId() != null)
		{
			return verdeling.getId().toString();
		}
		else
		{
			String transientId = "";
			if (verdeling.getColoscopieCentrum() != null)
			{
				transientId += verdeling.getColoscopieCentrum().getId().toString();
			}
			if (verdeling.getUitnodigingsGebied() != null)
			{
				if (!transientId.isEmpty())
				{
					transientId += "_";
				}
				transientId += verdeling.getUitnodigingsGebied().getId().toString();
			}
			return transientId;
		}
	}

	public static boolean isIfobtActief(Client andereClient, List<Long> uitgenodigdeClientIds)
	{

		if (uitgenodigdeClientIds.contains(andereClient.getId()))
		{
			return true;
		}

		if (andereClient.getColonDossier().getLaatsteScreeningRonde() == null
			|| andereClient.getColonDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging() == null)
		{
			return false;
		}

		IFOBTTest ifobtTest = andereClient.getColonDossier().getLaatsteScreeningRonde().getLaatsteIFOBTTest();
		if (ifobtTest == null)
		{
			return true;
		}

		return ifobtTest.getStatus() == IFOBTTestStatus.ACTIEF;
	}

	public static boolean isWachttijdOpPakketVerstreken(Client andereClient, Integer wachttijdVerzendenPakket, List<Long> uitgenodigdeClientIds, LocalDate vandaag)
	{
		if (andereClient.getColonDossier().getLaatsteScreeningRonde() != null
			&& andereClient.getColonDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging() != null
			&& andereClient.getColonDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging().getCreatieDatum() != null)
		{

			if (uitgenodigdeClientIds.contains(andereClient.getId()))
			{
				return false;
			}

			LocalDate createDatumUitnodiging = DateUtil.toLocalDate(andereClient.getColonDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging().getCreatieDatum());
			createDatumUitnodiging = createDatumUitnodiging.plusDays(wachttijdVerzendenPakket);
			return !createDatumUitnodiging.isAfter(vandaag);
		}
		return false;
	}

	public static Client getAndereClient(List<Client> clientenOpAdres, Client item)
	{
		for (Client client : clientenOpAdres)
		{
			if (!client.getId().equals(item.getId()))
			{
				return client;
			}
		}
		return null;
	}

	public static Criterion critOngunstig(String alias)
	{
		alias = ScreenitRestrictions.fixAlias(alias);
		return Restrictions.or(Restrictions.geProperty(alias + "uitslag", alias + "normWaarde"),
			Restrictions.eq(alias + "geinterpreteerdeUitslag", ColonGeinterpreteerdeUitslag.ONGUNSTIG));
	}

	public static Criterion critOngunstig()
	{
		return critOngunstig("");
	}

	public static void addNogGeenUitslagbriefOntvangenCriteria(Criteria crit, String rondeAlias)
	{
		addNogGeenUitslagbriefOntvangenCriteria(crit, rondeAlias, BriefType.COLON_UITSLAG_BRIEVEN);
	}

	public static void addNogGeenUitslagbriefOntvangenCriteria(Criteria crit, String rondeAlias, List<BriefType> uitslagBriefTypes)
	{
		rondeAlias = ScreenitRestrictions.fixAlias(rondeAlias);
		DetachedCriteria subquery = DetachedCriteria.forClass(ColonBrief.class, "brief");
		subquery.setProjection(Projections.id());
		subquery.add(Restrictions.eqProperty("brief.screeningRonde", rondeAlias + "id"));
		subquery.add(Restrictions.in("brief.briefType", uitslagBriefTypes));
		crit.add(Subqueries.notExists(subquery));
	}

	public static void addIfobtMissendeUitslagRestrictions(Criteria criteria, LocalDate signalerenVanaf, LocalDate minimaleSignaleringsDatum)
	{
		criteria.createAlias("ifobt.colonScreeningRonde", "ronde");
		criteria.createAlias("ronde.dossier", "dossier");
		criteria.createAlias("dossier.client", "client");
		criteria.createAlias("client.persoon", "persoon");

		criteria.add(Restrictions.le("ifobt.statusDatum", DateUtil.toUtilDate(minimaleSignaleringsDatum)));
		criteria.add(Restrictions.eq("ifobt.type", GOLD));

		criteria.add(
			Restrictions.or(
				Restrictions.and(
					Restrictions.gt("dossier.datumLaatstGecontroleerdeSignalering", DateUtil.toUtilDate(signalerenVanaf)),
					DateRestrictions.gtProperty("ifobt.analyseDatum", "dossier.datumLaatstGecontroleerdeSignalering")
				),
				Restrictions.and(
					Restrictions.or(
						DateRestrictions.le("dossier.datumLaatstGecontroleerdeSignalering", DateUtil.toUtilDate(signalerenVanaf)),
						Restrictions.isNull("dossier.datumLaatstGecontroleerdeSignalering")
					),
					Restrictions.gt("ifobt.analyseDatum", DateUtil.toUtilDate(signalerenVanaf))
				)
			)
		);

		var uitslagBriefSubQuery = DetachedCriteria.forClass(ColonBrief.class, "brief");
		uitslagBriefSubQuery.setProjection(Projections.id());
		uitslagBriefSubQuery.createAlias("brief.projectBrief", "projectBrief", JoinType.LEFT_OUTER_JOIN);
		uitslagBriefSubQuery.add(Restrictions.eqProperty("brief.ifobtTest", "ifobt.id"));
		uitslagBriefSubQuery.add(
			Restrictions.or(
				Restrictions.and(
					Restrictions.eq("brief.vervangendeProjectBrief", false),
					Restrictions.in("brief.briefType", BriefType.COLON_UITSLAG_BRIEVEN),
					Restrictions.eq("brief.gegenereerd", true)
				),
				Restrictions.and(
					Restrictions.eq("brief.vervangendeProjectBrief", true),
					Restrictions.in("projectBrief.briefType", BriefType.COLON_UITSLAG_BRIEVEN),
					Restrictions.eq("projectBrief.gegenereerd", true)
				)
			)
		);

		var nieuweUitnodigingSubQuery = uitnodigingenVanClientCreatieDatumGelijkStatusDatumIfobt();

		var nieuweUitnodigingZonderGekoppeldeFITSubQuery = uitnodigingenVanClientCreatieDatumGelijkStatusDatumIfobt();
		nieuweUitnodigingZonderGekoppeldeFITSubQuery.add(Restrictions.isNull("uitnodiging.gekoppeldeTest"));

		criteria.add(Restrictions.or(
			Restrictions.and(
				Restrictions.eq("ifobt.status", IFOBTTestStatus.UITGEVOERD),
				Subqueries.notExists(uitslagBriefSubQuery)
			),
			Restrictions.and(
				Restrictions.in("ifobt.status", Arrays.asList(IFOBTTestStatus.VERVALDATUMVERLOPEN, IFOBTTestStatus.NIETTEBEOORDELEN)),
				Restrictions.or(
					Restrictions.and(
						Subqueries.notExists(nieuweUitnodigingSubQuery),
						Subqueries.notExists(uitslagBriefSubQuery)
					),
					Subqueries.exists(nieuweUitnodigingZonderGekoppeldeFITSubQuery)
				)
			)
		));

		ScreenitRestrictions.addClientBaseRestrictions(criteria, "client", "persoon");
	}

	private static DetachedCriteria uitnodigingenVanClientCreatieDatumGelijkStatusDatumIfobt()
	{
		var nieuweUitnodigingSubQuery = DetachedCriteria.forClass(ColonUitnodiging.class, "uitnodiging");
		nieuweUitnodigingSubQuery.setProjection(Projections.id());
		nieuweUitnodigingSubQuery.createAlias("uitnodiging.screeningRonde", "screeningRonde");
		nieuweUitnodigingSubQuery.add(Restrictions.eqProperty("screeningRonde.dossier", "dossier.id"));
		nieuweUitnodigingSubQuery.add(DateRestrictions.eqProperty("uitnodiging.creatieDatum", "ifobt.statusDatum"));
		return nieuweUitnodigingSubQuery;
	}

	public static Criterion critGunstig(String alias)
	{
		alias = ScreenitRestrictions.fixAlias(alias);
		return Restrictions.ltProperty(alias + "uitslag", alias + "normWaarde");
	}

	public static void addHeeftGeenAfgerondeVerlagenRestrictions(Criteria criteria, String rondeAlias)
	{
		DetachedCriteria verslagenCrit = DetachedCriteria.forClass(ColonVerslag.class);
		verslagenCrit.add(Restrictions.eq("status", VerslagStatus.AFGEROND));
		verslagenCrit.setProjection(Projections.distinct(Projections.property("screeningRonde.id")));
		criteria.add(Subqueries.propertyNotIn(rondeAlias + "id", verslagenCrit));
	}

	private static class SpecialPropertyExpression implements Criterion
	{
		private static final long serialVersionUID = 1L;

		private static final TypedValue[] NO_TYPED_VALUES = new TypedValue[0];

		private String propertyName;

		private String otherPropertyName;

		private String op;

		private String afwijkingOther;

		protected SpecialPropertyExpression(String propertyName, String op, String otherPropertyName, String afwijkingOther)
		{
			this.propertyName = propertyName;
			this.otherPropertyName = otherPropertyName;
			this.op = op;
			this.afwijkingOther = afwijkingOther;
		}

		@Override
		public String toSqlString(Criteria criteria, CriteriaQuery criteriaQuery) throws HibernateException
		{
			final String[] lhsColumns = criteriaQuery.findColumns(propertyName, criteria);
			final String[] rhsColumns = criteriaQuery.findColumns(otherPropertyName, criteria);

			final String[] comparisons = { lhsColumns[0] + op + "(" + rhsColumns[0] + afwijkingOther + ")" };
			if (comparisons.length > 1)
			{
				return '(' + String.join(" and ", comparisons) + ')';
			}
			else
			{
				return comparisons[0];
			}
		}

		@Override
		public TypedValue[] getTypedValues(Criteria criteria, CriteriaQuery criteriaQuery) throws HibernateException
		{
			return NO_TYPED_VALUES;
		}

		@Override
		public String toString()
		{
			return propertyName + op + otherPropertyName;
		}

	}

}
