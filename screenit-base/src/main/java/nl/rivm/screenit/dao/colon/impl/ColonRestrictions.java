package nl.rivm.screenit.dao.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonGeinterpreteerdeUitslag;
import nl.rivm.screenit.model.colon.ColonVerslag;
import nl.rivm.screenit.model.colon.ColoscopieCentrumColonCapaciteitVerdeling;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.UitnodigingsGebied;
import nl.rivm.screenit.model.colon.enums.ColonAfspraakStatus;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.specification.algemeen.ClientSpecification;
import nl.rivm.screenit.specification.colon.ColonUitnodigingBaseSpecification;
import nl.rivm.screenit.util.DateUtil;
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
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.hibernate.engine.spi.TypedValue;
import org.hibernate.sql.JoinType;

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

		addProjectMetPrioriteitCriteria(crit, projectGroupId, exclusieGroepIds);

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

	public static Criteria getQueryU2(Session session, UitnodigingsGebied uitnodigingsGebied, Integer minimaleLeeftijd, Integer maximaleLeeftijd, boolean count,
		Long projectGroupId,
		List<Long> exclusieGroepIds, LocalDate vandaag)
	{
		Criteria crit = getCriteriaUitnodigen(session, uitnodigingsGebied, minimaleLeeftijd, maximaleLeeftijd, vandaag);

		crit.createAlias("dossier.volgendeUitnodiging", "volgendeUitnodiging", JoinType.INNER_JOIN);
		crit.createAlias("volgendeUitnodiging.interval", "interval", JoinType.INNER_JOIN);
		crit.createAlias("laatsteScreeningRonde.laatsteAfspraak", "afspraak", JoinType.LEFT_OUTER_JOIN);

		crit.add(getU2BaseCriteria(vandaag));

		crit.add(Restrictions.eq("dossier.wachtOpStartProject", false));

		addProjectMetPrioriteitCriteria(crit, projectGroupId, exclusieGroepIds);

		if (!count)
		{
			crit.addOrder(Order.asc("laatsteScreeningRonde.creatieDatum"));
			addDefaultOrder(crit);
		}

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
				Restrictions.ne("afspraak.status", ColonAfspraakStatus.GEPLAND),
				Restrictions.le("afspraak.vanaf", DateUtil.minusWerkdagen(peildatum, 5).atStartOfDay())));
	}

	private static void addProjectMetPrioriteitCriteria(Criteria crit, Long projectGroupId, List<Long> exclusieGroepIds)
	{
		if (projectGroupId != null)
		{
			crit.createAlias("projecten", "projectClient");
			crit.add(Restrictions.eq("projectClient.groep.id", projectGroupId));
			crit.add(Restrictions.eq("projectClient.actief", true));
		}
		else if (CollectionUtils.isNotEmpty(exclusieGroepIds))
		{
			DetachedCriteria critProjectClienten = DetachedCriteria.forClass(ProjectClient.class, "projectClient");
			critProjectClienten.add(Restrictions.eq("projectClient.actief", true));
			critProjectClienten.add(Restrictions.in("projectClient.groep.id", exclusieGroepIds));
			critProjectClienten.add(Restrictions.eqProperty("projectClient.client", "rootClient.id"));
			critProjectClienten.setProjection(Projections.id());
			crit.add(Subqueries.notExists(critProjectClienten));
		}
	}

	private static Criterion createReferentieCriteria(LocalDate peildatum, LocalDate vandaag)
	{
		long afwijking = ChronoUnit.DAYS.between(vandaag, peildatum);
		Criterion referentieCriteria =
			Restrictions.or(
				Restrictions.and(
					Restrictions.isNotNull("volgendeUitnodiging.datumVolgendeRonde"),
					Restrictions.le("volgendeUitnodiging.datumVolgendeRonde", vandaag)
				),
				Restrictions.and(
					Restrictions.isNull("volgendeUitnodiging.datumVolgendeRonde"),
					Restrictions.or(
						Restrictions.and(
							Restrictions.isNull("volgendeUitnodiging.projectPeildatum"),
							Restrictions.leProperty("volgendeUitnodiging.peildatum", "interval.berekendeReferentieDatum")
						),
						Restrictions.and(
							Restrictions.isNotNull("volgendeUitnodiging.projectPeildatum"),
							Restrictions.leProperty("volgendeUitnodiging.projectPeildatum", "interval.berekendeReferentieDatum")
						)
					)
				)
			);
		if (afwijking != 0)
		{
			referentieCriteria =
				Restrictions.or(
					Restrictions.and(
						Restrictions.isNotNull("volgendeUitnodiging.datumVolgendeRonde"),
						Restrictions.le("volgendeUitnodiging.datumVolgendeRonde", vandaag.plusDays(afwijking))
					),
					Restrictions.and(
						Restrictions.isNull("volgendeUitnodiging.datumVolgendeRonde"),
						Restrictions.or(
							Restrictions.and(
								Restrictions.isNull("volgendeUitnodiging.projectPeildatum"),
								new SpecialPropertyExpression("volgendeUitnodiging.peildatum", "<=", "interval.berekendeReferentieDatum", " + interval '" + afwijking + "' day")),
							Restrictions.and(
								Restrictions.isNotNull("volgendeUitnodiging.projectPeildatum"),
								new SpecialPropertyExpression("volgendeUitnodiging.projectPeildatum", "<=", "interval.berekendeReferentieDatum",
									" + interval '" + afwijking + "' day"))
						)
					)
				);
		}
		return referentieCriteria;
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
			if (verdeling.getIntakelocatie() != null)
			{
				transientId += verdeling.getIntakelocatie().getId().toString();
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

	@Deprecated(forRemoval = true)
	public static void addNogGeenUitslagbriefOntvangenCriteria(Criteria crit, String rondeAlias)
	{
		addNogGeenBriefOntvangenVanTypesCriteria(crit, rondeAlias, BriefType.COLON_UITSLAG_BRIEVEN);
	}

	@Deprecated(forRemoval = true)
	public static void addNogGeenBriefOntvangenVanTypesCriteria(Criteria crit, String rondeAlias, List<BriefType> briefTypes)
	{
		rondeAlias = ScreenitRestrictions.fixAlias(rondeAlias);
		DetachedCriteria subquery = DetachedCriteria.forClass(ColonBrief.class, "brief");
		subquery.setProjection(Projections.id());
		subquery.add(Restrictions.eqProperty("brief.screeningRonde", rondeAlias + "id"));
		subquery.add(Restrictions.in("brief.briefType", briefTypes));
		crit.add(Subqueries.notExists(subquery));
	}

	@Deprecated(forRemoval = true)
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

		private final String propertyName;

		private final String otherPropertyName;

		private final String op;

		private final String afwijkingOther;

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
