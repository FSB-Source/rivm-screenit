package nl.rivm.screenit.dao.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dao.colon.IntakelocatieVanTotEnMetFilter;
import nl.rivm.screenit.dao.colon.RoosterDao;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.RangeCriteriaBuilder;
import nl.rivm.screenit.model.berichten.cda.MeldingOngeldigCdaBericht;
import nl.rivm.screenit.model.berichten.enums.BerichtType;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.RoosterItemListViewWrapper;
import nl.rivm.screenit.model.colon.RoosterListViewFilter;
import nl.rivm.screenit.model.colon.enums.ColonTijdSlotType;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.colon.planning.ColonBlokkade;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.rivm.screenit.model.colon.planning.VrijSlotZonderKamer;
import nl.rivm.screenit.model.colon.planning.VrijSlotZonderKamerFilter;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.criteria.BaseCriteria;
import nl.topicuszorg.hibernate.criteria.ListCriteria;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.AbstractRecurrence;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.NoRecurrence;
import nl.topicuszorg.wicket.planning.util.Periode;

import org.hibernate.Criteria;
import org.hibernate.SQLQuery;
import org.hibernate.criterion.Conjunction;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Disjunction;
import org.hibernate.criterion.MatchMode;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.ProjectionList;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.hibernate.transform.Transformers;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.Range;
import com.google.common.primitives.Ints;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
@Slf4j
public class RoosterDaoImpl extends AbstractAutowiredDao implements RoosterDao
{
	public static final String[] HERVERWERKING_MARKERS = new String[] { "zonder conclusie", "zonder passende screeningsronde", "geen ongunstige uitslag" };

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public <T extends AbstractAppointment> List<T> getAppointments(Periode periode, List<Kamer> kamers, Class<T> type)
	{
		BaseCriteria<T> criteria = createCriteria(periode, type);

		if (kamers != null && kamers.size() > 0)
		{
			criteria.add(Restrictions.in("location", kamers));
		}
		criteria.addOrder(Order.asc("startTime"));
		return criteria.list(getSession());
	}

	private <T extends AbstractAppointment> BaseCriteria<T> createCriteria(Periode periode, Class<T> type)
	{
		BaseCriteria<T> criteria = new BaseCriteria<>(type);
		criteria.add(RangeCriteriaBuilder.closedOpen("startTime", "endTime").overlaps(Range.closed(periode.getT(), periode.getU())));
		return criteria;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<RoosterItem> getOneindigeItems()
	{
		Criteria criteria = getSession().createCriteria(RoosterItem.class);

		criteria.add(Restrictions.isNotNull("recurrence"));

		criteria.createAlias("recurrence", "r");
		criteria.add(Restrictions.isNull("r.endDate"));

		DetachedCriteria sub = DetachedCriteria.forClass(RoosterItem.class);

		sub.createAlias("recurrence", "subRecurrence");
		sub.add(Restrictions.eqProperty("r.id", "subRecurrence.id"));

		sub.setProjection(Projections.max("startTime"));
		criteria.add(Subqueries.propertyEq("startTime", sub));

		return criteria.list();
	}

	@Override
	public List<Object> getRoosterTijden(List<Range<Date>> ranges, RoosterItem roosteritem, Range<Date> totaalInterval)
	{
		Criteria criteria = getSession().createCriteria(RoosterItem.class);

		Disjunction disjunction = Restrictions.disjunction();
		for (var range : ranges)
		{
			disjunction.add(RangeCriteriaBuilder.closedOpen("startTime", "endTime").overlaps(range));
		}
		criteria.add(disjunction);
		criteria.addOrder(Order.asc("startTime"));

		criteria.add(Restrictions.eq("location", roosteritem.getLocation()));

		AbstractRecurrence recurrence = roosteritem.getRecurrence();
		if (recurrence != null && recurrence.getId() != null && !NoRecurrence.class.isAssignableFrom(recurrence.getClass()))
		{
			Conjunction conjunction = Restrictions.conjunction();
			conjunction.add(Restrictions.eq("recurrence", recurrence));
			if (totaalInterval != null)
			{
				conjunction.add(Restrictions.ge("startTime", totaalInterval.lowerEndpoint()));
				conjunction.add(Restrictions.lt("endTime", totaalInterval.upperEndpoint()));
			}
			disjunction.add(conjunction);
		}
		else if (roosteritem.getId() != null)
		{
			disjunction.add(Restrictions.eq("id", roosteritem.getId()));
		}

		ProjectionList projectionList = Projections.projectionList()
			.add(Projections.property("startTime"))
			.add(Projections.property("endTime"))
			.add(Projections.property("id"));

		criteria.setProjection(projectionList);

		return criteria.list();
	}

	@Override
	public List<RoosterItemListViewWrapper> getAlleRoosterBlokkenInPeriode(String sortProperty, boolean asc, RoosterListViewFilter filter,
		ColoscopieCentrum intakeLocatie)
	{
		var criteria = createRoosterBlokkenCriteria(filter, intakeLocatie, sortProperty, asc);
		return criteria.list();
	}

	@Override
	public List<RoosterItemListViewWrapper> getRoosterBlokken(String sortProperty, boolean asc, long first, long count, RoosterListViewFilter filter,
		ColoscopieCentrum intakeLocatie)
	{
		SQLQuery criteria = createRoosterBlokkenCriteria(filter, intakeLocatie, sortProperty, asc);
		if (first >= 0)
		{
			criteria.setFirstResult(Ints.checkedCast(first));
		}
		if (count >= 0)
		{
			criteria.setMaxResults(Ints.checkedCast(count));
		}

		return criteria.list();
	}

	private SQLQuery createRoosterBlokkenCriteria(RoosterListViewFilter filter, ColoscopieCentrum intakeLocatie, String sortProperty, boolean asc)
	{
		getSession().flush();
		String selectFromQueryString = "select ";
		if (sortProperty == null)
		{
			selectFromQueryString += "count(ri.id) ";
		}
		else
		{
			selectFromQueryString += "pa.start_time as \"startTime\", pa.end_time as \"endTime\", k.name as \"kamer\", k.id as \"kamerId\", ri.id as \"roosterItemId\", ri.capaciteit_mee_bepaald as \"capaciteitMeeBepaald\"";
		}
		selectFromQueryString += " from colon.rooster_item ri "
			+ "inner join colon.plan_appointment pa on ri.id=pa.id " 
			+ "inner join colon.plan_location k on pa.location=k.id " 
			+ "inner join algemeen.org_organisatie il on il.id=k.coloscopie_centrum "; 
		String whereQueryString = "where il.id=:intakelocatie and il.actief = true and k.actief = true ";

		Map<String, Object> params = new HashMap<String, Object>();

		if (filter.getStatus() != null)
		{
			switch (filter.getStatus())
			{
			case INTAKE_GEPLAND:
				selectFromQueryString += "inner join colon.afspraak a on ri.id=a.rooster_item ";
				whereQueryString += "and (a.status=:status1 or a.status=:status2) ";
				params.put("status1", AfspraakStatus.GEPLAND.name());
				params.put("status2", AfspraakStatus.UITGEVOERD.name());
				break;
			case GEBRUIKT_VOOR_CAPACITEIT:
				whereQueryString += "and ri.capaciteit_mee_bepaald = true ";
				selectFromQueryString += "left outer join colon.afspraak a on ri.id=a.rooster_item ";
				whereQueryString += "and (a.status is null or (a.status!=:status1 and a.status!=:status2)) ";
				params.put("status1", AfspraakStatus.GEPLAND.name());
				params.put("status2", AfspraakStatus.UITGEVOERD.name());
				whereQueryString += "and not EXISTS(select b.id from colon.plan_appointment b where b.title='Blokkade' and b.location=k.id and b.start_time<pa.end_time and b.end_time>pa.start_time) ";
				break;
			case VRIJ_TE_VERPLAATSEN:
				whereQueryString += "and not exists(select id from colon.afspraak ia where ia.rooster_item = ri.id and (ia.status=:status1 or ia.status=:status2)) " 
					+ "and not EXISTS(select b.id from colon.plan_appointment b where b.title='Blokkade' and b.location=k.id and b.start_time<pa.end_time and b.end_time>pa.start_time) ";
				if (filter.getRekeningHoudenMetCapaciteitMeeBepaald())
				{
					whereQueryString += "and ri.capaciteit_mee_bepaald = false";
				}
				params.put("status1", AfspraakStatus.GEPLAND.name());
				params.put("status2", AfspraakStatus.UITGEVOERD.name());
				break;
			case BLOKKADE:
				whereQueryString += "and EXISTS(select b.id from colon.plan_appointment b where b.title='Blokkade' and b.location=k.id and b.start_time<pa.end_time and b.end_time>pa.start_time) ";
				break;
			default:
				break;
			}
		}
		whereQueryString += " and pa.end_time>:startTime and pa.start_time<:endTime ";
		String orderByQueryString = "";
		if (sortProperty != null)
		{
			orderByQueryString += "order by ";
			boolean supportedSortProperty = true;
			switch (sortProperty)
			{
			case "startTime":
				orderByQueryString += "pa.start_time ";
				break;
			case "endTime":
				orderByQueryString += "pa.end_time ";
				break;
			case "kamer":
				orderByQueryString += "k.name ";
				break;
			default:
				LOG.error("unknown sortProperty " + sortProperty + " filter " + filter.getStatus());
				supportedSortProperty = false;
				break;
			}
			if (supportedSortProperty)
			{
				if (asc)
				{
					orderByQueryString += "asc";
				}
				else
				{
					orderByQueryString += "desc";
				}
			}
		}

		SQLQuery criteria = getSession().createSQLQuery(selectFromQueryString + whereQueryString + orderByQueryString);
		criteria
			.setParameter("endTime", filter.getEndDatum()) 
			.setParameter("startTime", filter.getStartDatum()) 
			.setParameter("intakelocatie", intakeLocatie.getId());

		for (Entry<String, Object> param : params.entrySet())
		{
			criteria.setParameter(param.getKey(), param.getValue());
		}
		if (sortProperty != null)
		{
			criteria.setResultTransformer(Transformers.aliasToBean(RoosterItemListViewWrapper.class));
		}

		return criteria;
	}

	@Override
	public List<VrijSlotZonderKamer> getVrijeSlotenZonderKamer(String sortProperty, boolean asc, long first, long count, VrijSlotZonderKamerFilter filter)
	{
		SQLQuery query = createVrijSlotZonderKamerQuery(filter, sortProperty, asc);
		if (first >= 0)
		{
			query.setFirstResult(Ints.checkedCast(first));
		}
		if (count >= 0)
		{
			query.setMaxResults(Ints.checkedCast(count));
		}
		return query.list();
	}

	@Override
	public List<VrijSlotZonderKamer> getVrijeSlotenZonderKamer(String sortProperty, boolean asc, VrijSlotZonderKamerFilter filter)
	{
		SQLQuery query = createVrijSlotZonderKamerQuery(filter, sortProperty, asc);
		return query.list();
	}

	@Override
	public List<VrijSlotZonderKamer> getVrijeSlotenZonderKamer(VrijSlotZonderKamerFilter filter)
	{
		SQLQuery query = createVrijSlotZonderKamerQuery(filter, "niet", true);
		return query.list();
	}

	@Override
	public long getVrijeSlotenZonderKamerCount(VrijSlotZonderKamerFilter filter)
	{
		SQLQuery query = createVrijSlotZonderKamerQuery(filter, null, true);
		return ((Number) query.uniqueResult()).longValue();
	}

	private SQLQuery createVrijSlotZonderKamerQuery(VrijSlotZonderKamerFilter filter, String sortProperty, boolean asc)
	{
		getSession().flush();

		StringBuilder querySB = new StringBuilder();

		querySB.append("WITH bezoek_adres AS");
		querySB.append("(");
		querySB.append("	WITH organisatie_adressn_met_rank AS");
		querySB.append("	(");
		querySB.append("		SELECT");
		querySB.append("		row_number() OVER (PARTITION BY org_organisatie ORDER BY adressen) AS rank,");
		querySB.append("		org_organisatie, adressen");
		querySB.append("		FROM algemeen.org_organisatie_adressen");
		querySB.append("	)");
		querySB.append("	SELECT org_organisatie, adressen");
		querySB.append("	FROM organisatie_adressn_met_rank");
		querySB.append("	WHERE rank = 1");
		querySB.append(")");

		if (!Boolean.TRUE.equals(filter.getAlleenIntakeLokaties()))
		{
			if (sortProperty == null)
			{
				querySB.append(" select count(distinct(pa.start_time, cc.id))");
			}
			else
			{
				querySB.append(
					" select distinct pa.start_time as \"startTijd\", pa.end_time as \"eindTijd\",  cc.id as \"intakeLocatieId\", a.plaats as \"plaats\", cc.naam as \"naam\"");
			}
		}
		else
		{
			if (sortProperty == null)
			{
				querySB.append(" select count(distinct(cc.id))");
			}
			else
			{
				querySB.append(" select distinct cc.id as \"intakeLocatieId\", a.plaats as \"plaats\", cc.naam as \"naam\"");
			}
		}

		if (!Boolean.TRUE.equals(filter.getAlleenIntakeLokaties()))
		{
			querySB.append(" from colon.rooster_item ri");
			querySB.append(" inner join colon.plan_appointment pa on ri.id=pa.id");
			querySB.append(" inner join colon.plan_location k on pa.location=k.id");
		}
		else
		{
			querySB.append(" from colon.plan_location k");
		}
		querySB.append(" inner join algemeen.org_organisatie cc on k.coloscopie_centrum=cc.id");
		querySB.append(" inner join bezoek_adres ba on cc.id=ba.org_organisatie");
		querySB.append(" inner join gedeeld.org_adres a on ba.adressen=a.id");

		Map<String, Object> params = new HashMap<String, Object>();
		querySB.append(" where cc.actief = true");
		querySB.append(" and k.actief = true");
		if (!Boolean.TRUE.equals(filter.getAlleenIntakeLokaties()))
		{
			querySB.append(" and not exists(select id from colon.afspraak ia where ia.rooster_item = ri.id and (ia.status=:status1 or ia.status=:status2))");
			params.put("status1", AfspraakStatus.GEPLAND.name());
			params.put("status2", AfspraakStatus.UITGEVOERD.name());
			querySB.append(" and pa.id not in (select pa2.id from colon.plan_appointment b, colon.plan_appointment pa2 "
				+ "where b.title='" + ColonTijdSlotType.BLOKKADE.getTitle() + "' and pa2.title='" + ColonTijdSlotType.ROOSTER_ITEM.getTitle()
				+ "' and b.location=pa2.location and b.start_time<pa2.end_time and b.end_time>pa2.start_time and pa2.start_time>:vanaf1 and pa2.start_time<:totEnMet1 )");

			querySB.append(" and pa.start_time>:vanaf and pa.start_time<:totEnMet");
			params.put("vanaf", filter.getVanaf());
			params.put("totEnMet", DateUtil.plusDagen(filter.getTotEnMet(), 1));
			params.put("vanaf1", filter.getVanaf());
			params.put("totEnMet1", DateUtil.plusDagen(filter.getTotEnMet(), 1));
			if (filter.getIntakeLocatieId() != null)
			{
				querySB.append(" and cc.id = :intakeLocatieId");
				params.put("intakeLocatieId", filter.getIntakeLocatieId());
			}
			if (filter.getNietIntakeLocatieId() != null)
			{
				querySB.append(" and cc.id != :nietIntakeLocatieId");
				params.put("nietIntakeLocatieId", filter.getNietIntakeLocatieId());
			}
		}

		if (filter.getNaam() != null && !filter.getNaam().isEmpty())
		{
			querySB.append(" and cc.naam ILIKE '%' || :naam || '%'");
			params.put("naam", filter.getNaam());
		}
		if (filter.getPlaats() != null && !filter.getPlaats().isEmpty())
		{
			querySB.append(" and plaats ILIKE '%' || :plaats || '%'");
			params.put("plaats", filter.getPlaats());
		}

		if (sortProperty != null && !sortProperty.equals("niet"))
		{
			querySB.append(" order by");
			switch (sortProperty)
			{
			case "startTime":
				querySB.append(" \"startTijd\"");
				break;
			case "naam":
				querySB.append(" \"naam\"");
				break;
			case "plaats":
				querySB.append(" \"plaats\"");
				break;
			default:
				LOG.error("unknown sortProperty " + sortProperty);
				break;
			}
			if (asc)
			{
				querySB.append(" asc");
			}
			else
			{
				querySB.append(" desc");
			}
		}

		SQLQuery query = getSession().createSQLQuery(querySB.toString());
		for (Entry<String, Object> param : params.entrySet())
		{
			query.setParameter(param.getKey(), param.getValue());
		}
		if (sortProperty != null)
		{
			query.setResultTransformer(Transformers.aliasToBean(VrijSlotZonderKamer.class));
		}
		return query;
	}

	@Override
	public List<Kamer> getKamers(Date startTijd, Long intakeLocatieId)
	{
		StringBuilder querySB = new StringBuilder();

		querySB.append("select {k.*}");

		querySB.append(" from colon.rooster_item ri");
		querySB.append(" inner join colon.plan_appointment pa on ri.id=pa.id");
		querySB.append(" inner join colon.plan_location k on pa.location=k.id");

		Map<String, Object> params = new HashMap<String, Object>();
		querySB.append(" and k.actief = true");
		querySB.append(" and not exists(select id from colon.afspraak ia where ia.rooster_item = ri.id and (ia.status=:status1 or ia.status=:status2))");
		params.put("status1", AfspraakStatus.GEPLAND.name());
		params.put("status2", AfspraakStatus.UITGEVOERD.name());
		querySB.append(
			" and not EXISTS(select b.id from colon.plan_appointment b where b.title='" + ColonTijdSlotType.BLOKKADE.getTitle()
				+ "' and b.location=pa.location and b.start_time<pa.end_time and b.end_time>pa.start_time)");
		querySB.append(" and pa.start_time=:startTijd");
		params.put("startTijd", startTijd);
		querySB.append(" and k.coloscopie_centrum = :intakeLocatieId");
		params.put("intakeLocatieId", intakeLocatieId);

		SQLQuery query = getSession().createSQLQuery(querySB.toString()).addEntity("k", Kamer.class);
		for (Entry<String, Object> param : params.entrySet())
		{
			query.setParameter(param.getKey(), param.getValue());
		}
		return query.list();
	}

	@Override
	public long getRoosterBlokkenCount(RoosterListViewFilter filter, ColoscopieCentrum intakeLocatie)
	{
		SQLQuery criteria = createRoosterBlokkenCriteria(filter, intakeLocatie, null, true);

		return ((Number) criteria.uniqueResult()).longValue();
	}

	@Override
	public List<Object> getCurrentRoosterBlokken(Kamer kamer, Range<Date> periode)
	{
		Criteria criteria = getSession().createCriteria(RoosterItem.class);
		criteria.createAlias("location", "location");
		criteria.add(Restrictions.eq("location", kamer));
		criteria.add(RangeCriteriaBuilder.closedOpen("startTime", "endTime").overlaps(periode));
		criteria.setProjection(Projections.projectionList().add(Projections.property("startTime")).add(Projections.property("endTime")));
		return criteria.list();
	}

	@Override
	public List<Date> getMdlDatums(Client client, IntakelocatieVanTotEnMetFilter intakeVanTotEnMetFilter)
	{
		Set<Date> datums = new HashSet<>();
		Date van = DateUtil.toUtilDate(currentDateSupplier.getLocalDate().minusYears(20));
		if (intakeVanTotEnMetFilter.getVanaf() != null)
		{
			van = intakeVanTotEnMetFilter.getVanaf();
		}
		Date tot = currentDateSupplier.getDate();
		if (intakeVanTotEnMetFilter.getTotEnMet() != null)
		{
			tot = DateUtil.plusDagen(intakeVanTotEnMetFilter.getTotEnMet(), 1);
		}

		BaseCriteria<Date> subQuery1 = new BaseCriteria<>(MeldingOngeldigCdaBericht.class);
		subQuery1.add(Restrictions.eq("actief", true));
		Disjunction markers = Restrictions.disjunction();
		for (String marker : HERVERWERKING_MARKERS)
		{
			markers.add(Restrictions.ilike("melding", marker, MatchMode.ANYWHERE));
		}
		subQuery1.add(markers);
		subQuery1.add(Restrictions.eq("bsn", client.getPersoon().getBsn()));
		subQuery1.createAlias("ontvangenCdaBericht", "ontvangenCdaBericht");
		subQuery1.add(Restrictions.eq("ontvangenCdaBericht.berichtType", BerichtType.MDL_VERSLAG));
		subQuery1.add(Restrictions.between("ontvangenCdaBericht.ontvangen", van, tot));
		subQuery1.setProjection(Projections.distinct(Projections.property("ontvangenCdaBericht.ontvangen")));

		datums.addAll(subQuery1.list(getSession()));

		BaseCriteria<Date> subQuery2 = new BaseCriteria<>(Client.class);
		subQuery2.add(Restrictions.eq("id", client.getId()));
		subQuery2.createAlias("colonDossier", "dossier");
		subQuery2.createAlias("dossier.laatsteScreeningRonde", "laatsteScreeningRonde");
		subQuery2.createAlias("laatsteScreeningRonde.verslagen", "verslag");
		subQuery2.add(Restrictions.eq("verslag.status", VerslagStatus.IN_BEWERKING));
		subQuery2.add(Restrictions.between("verslag.datumVerwerkt", van, tot));
		subQuery2.setProjection(Projections.distinct(Projections.property("verslag.datumVerwerkt")));

		datums.addAll(subQuery2.list(getSession()));

		return new ArrayList<Date>(datums);
	}

	@Override
	public List<ColonBlokkade> getBlokkades(Kamer kamer, Date startTime, Date endTime)
	{
		BaseCriteria<ColonBlokkade> criteria = createCriteria(new Periode(startTime, endTime), ColonBlokkade.class);
		criteria.createAlias("location", "location");
		criteria.add(Restrictions.eq("location", kamer));
		return criteria.list(getSession());
	}

	@Override
	public List<ColonBlokkade> getBlokkades(String sortProperty, boolean ascending, long first, long count, RoosterListViewFilter filter, ColoscopieCentrum intakelocatie)
	{
		BaseCriteria<ColonBlokkade> criteria = createCriteria(filter, intakelocatie);

		return criteria.list(getSession(), new ListCriteria(Ints.checkedCast(first), Ints.checkedCast(count), sortProperty, ascending));
	}

	private BaseCriteria<ColonBlokkade> createCriteria(RoosterListViewFilter filter, ColoscopieCentrum intakelocatie)
	{
		BaseCriteria<ColonBlokkade> criteria = createCriteria(
			new Periode(filter.getStartDatum(), DateUtil.plusDagen(DateUtil.startDag(filter.getEndDatum()), 1)), ColonBlokkade.class);
		criteria.createAlias("location", "kamer");
		criteria.add(Restrictions.eq("kamer.actief", true));
		criteria.add(Restrictions.eq("kamer.coloscopieCentrum", intakelocatie));
		return criteria;
	}

	@Override
	public long getBlokkadesCount(RoosterListViewFilter filter, ColoscopieCentrum intakelocatie)
	{
		BaseCriteria<ColonBlokkade> criteria = createCriteria(filter, intakelocatie);
		return criteria.countLong(getSession());
	}

}
