
package nl.rivm.screenit.dao.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.time.LocalDate;
import java.util.Arrays;
import java.util.Date;
import java.util.EnumSet;
import java.util.List;

import nl.rivm.screenit.dao.colon.AfspraakDao;
import nl.rivm.screenit.model.Afspraak;
import nl.rivm.screenit.model.RangeCriteriaBuilder;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.ConclusieTypeFilter;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.WerklijstIntakeFilter;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.planning.AfspraakLocatieWrapper;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.query.ScreenitRestrictions;
import nl.topicuszorg.hibernate.criteria.BaseCriteria;
import nl.topicuszorg.hibernate.criteria.ListCriteria;
import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;
import nl.topicuszorg.wicket.planning.dao.CriteriaHelper;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment;
import nl.topicuszorg.wicket.planning.model.appointment.Location;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.hibernate.Criteria;
import org.hibernate.Hibernate;
import org.hibernate.criterion.Conjunction;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Disjunction;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.ProjectionList;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.hibernate.envers.AuditReader;
import org.hibernate.envers.AuditReaderFactory;
import org.hibernate.envers.query.AuditEntity;
import org.hibernate.envers.query.AuditQuery;
import org.hibernate.sql.JoinType;
import org.hibernate.transform.Transformers;
import org.joda.time.DateTime;
import org.joda.time.Interval;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.primitives.Ints;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class AfspraakDaoImpl extends AbstractAutowiredDao implements AfspraakDao
{

	@Override
	public void saveOrUpdate(AbstractAppointment appointment)
	{
		getSession().saveOrUpdate(appointment);
	}

	@Override
	public void delete(AbstractAppointment appointment)
	{
		getSession().delete(appointment);
	}

	@Override
	public <T extends AbstractAppointment> List<T> getAfspraken(Date start, Date end, T filter, List<Location> locaties, EnumSet<AfspraakStatus> statusSet)
	{
		Criteria criteria = getSession().createCriteria(filter.getClass());

		if (filter instanceof Afspraak)
		{
			fillCriteria(criteria, (Afspraak) filter, start, end, statusSet, locaties);
		}
		else if (filter instanceof AbstractAppointment)
		{
			criteria.add(CriteriaHelper.datumFilterIn(start, end));
			fillCriteria(criteria, filter, locaties);
		}
		criteria.addOrder(Order.asc("startTime"));

		return criteria.list();

	}

	private void fillCriteria(Criteria criteria, Afspraak filter, Date start, Date end, EnumSet<AfspraakStatus> statusSet, List<Location> locaties)
	{
		if (statusSet != null && statusSet.equals(AfspraakStatus.VOOR_AGENDA))
		{
			criteria.add(Restrictions.or(Restrictions.eq("status", AfspraakStatus.GEPLAND), Restrictions.eq("status", AfspraakStatus.UITGEVOERD)));
		}
		else if (filter.getStatus() != null)
		{
			criteria.add(Restrictions.eq("status", filter.getStatus()));
		}
		if (filter.getLocation() != null)
		{
			criteria.add(Restrictions.eq("location", filter.getLocation()));
		}
		else if (CollectionUtils.isNotEmpty(locaties) && !locaties.contains(null))
		{
			criteria.add(Restrictions.in("location.id", CriteriaHelper.toIdList(locaties)));
		}
		else if (filter.getClient() != null)
		{
			criteria.add(Restrictions.eq("client", filter.getClient()));
		}

		datumFilterToevoegen(criteria, filter, start, end);
	}

	private void datumFilterToevoegen(Criteria criteria, Afspraak filter, Date start, Date end)
	{
		if (start != null && end != null)
		{
			criteria.add(CriteriaHelper.datumFilterIn(start, end));
		}
		else if (start != null)
		{
			criteria.add(Restrictions.ge("endTime", start));
		}
		else if (end != null)
		{
			criteria.add(Restrictions.le("startTime", end));
		}
	}

	private void fillCriteria(Criteria criteria, AbstractAppointment filter, List<Location> locaties)
	{
		if (filter.getLocation() != null)
		{
			criteria.add(Restrictions.eq("location", filter.getLocation()));
		}
		else if (CollectionUtils.isNotEmpty(locaties))
		{
			criteria.add(Restrictions.in("location", locaties));
		}
	}

	@Override
	public List<ColonIntakeAfspraak> getAfsprakenVoorColoscopiecentrum(WerklijstIntakeFilter zoekFilter, ColoscopieCentrum coloscopieCentrum, LocalDate vandaag, long first,
		long count, String property, boolean ascending)
	{
		BaseCriteria<? extends ColonIntakeAfspraak> criteria = createCriteria(zoekFilter, coloscopieCentrum, vandaag);
		if (property.startsWith("conclusie"))
		{
			criteria.alias("conclusie", JoinType.LEFT_OUTER_JOIN);
		}
		else if (property.startsWith("volgendeUitnodiging"))
		{
			criteria.alias("client");
			criteria.alias("client.colonDossier", "dossier");
			criteria.alias("dossier.volgendeUitnodiging", "volgendeUitnodiging");
		}

		if (!ascending)
		{
			criteria.addOrder(Order.desc(property));
			criteria.addOrder(Order.desc("id"));
		}
		else
		{
			criteria.addOrder(Order.asc(property));
			criteria.addOrder(Order.asc("id"));
		}

		List<ColonIntakeAfspraak> afspraken = (List<ColonIntakeAfspraak>) criteria.list(getSession(), new ListCriteria(Ints.checkedCast(first), Ints.checkedCast(count)));
		if (moetNogOpGeboortedatumFilteren(zoekFilter))
		{
			filterGeboortedatum(zoekFilter.getGeboortedatum(), afspraken);
		}
		return afspraken;
	}

	private boolean moetNogOpGeboortedatumFilteren(WerklijstIntakeFilter zoekFilter)
	{
		return AfspraakStatus.UITGEVOERD.equals(zoekFilter.getStatus()) && zoekFilter.getGeboortedatum() != null;
	}

	private BaseCriteria<? extends ColonIntakeAfspraak> createCriteria(WerklijstIntakeFilter zoekFilter, ColoscopieCentrum coloscopieCentrum, LocalDate vandaag)
	{
		BaseCriteria<ColonIntakeAfspraak> criteria = new BaseCriteria<ColonIntakeAfspraak>(ColonIntakeAfspraak.class);
		criteria.alias("location");
		criteria.add(Restrictions.eq("location.coloscopieCentrum", coloscopieCentrum));
		criteria.alias("client");
		criteria.alias("client.persoon", "persoon");
		criteria.alias("colonScreeningRonde");
		criteria.alias("client.colonDossier", "dossier");
		criteria.alias("conclusie", "conclusie", JoinType.LEFT_OUTER_JOIN);
		criteria.add(Restrictions.in("status", AfspraakStatus.VOOR_AGENDA));
		criteria.add(Restrictions.eqProperty("dossier.laatsteScreeningRonde", "colonScreeningRonde"));

		if (StringUtils.isNotBlank(zoekFilter.getBsn()) && (!AfspraakStatus.UITGEVOERD.equals(zoekFilter.getStatus()) || zoekFilter.getGeboortedatum() != null))
		{
			criteria.add(Restrictions.eq("persoon.bsn", zoekFilter.getBsn()));
		}
		else if (AfspraakStatus.UITGEVOERD.equals(zoekFilter.getStatus()))
		{

			criteria.add(Restrictions.eq("persoon.bsn", "nobsn"));
		}
		DetachedCriteria verslagenCrit = DetachedCriteria.forClass(MdlVerslag.class);
		verslagenCrit.add(Restrictions.eq("status", VerslagStatus.AFGEROND));
		verslagenCrit.setProjection(Projections.distinct(Projections.property("screeningRonde.id")));

		LocalDate vanaf = DateUtil.toLocalDate(zoekFilter.getVanaf());
		LocalDate totEnMet = null;
		if (zoekFilter.getTotEnMet() != null)
		{
			totEnMet = DateUtil.toLocalDate(zoekFilter.getTotEnMet()).plusDays(1);
		}
		if (AfspraakStatus.GEPLAND.equals(zoekFilter.getStatus()))
		{
			criteria.add(Restrictions.eq("status", AfspraakStatus.GEPLAND));
			if (vanaf == null || !vanaf.isAfter(vandaag))
			{
				vanaf = vandaag;
			}
			if (zoekFilter.getEersteKeerZoeken())
			{
				totEnMet = vandaag.plusDays(1);
			}
		}
		else if (AfspraakStatus.UITGEVOERD.equals(zoekFilter.getStatus()))
		{
			criteria.alias("colonScreeningRonde.ifobtTesten", "testen", JoinType.LEFT_OUTER_JOIN);
			criteria.alias("colonScreeningRonde.openUitnodiging", "openUitnodiging", JoinType.LEFT_OUTER_JOIN);
			criteria.add(
				Restrictions.or(
					Subqueries.propertyIn("colonScreeningRonde.id", verslagenCrit),
					Restrictions.and(
						Restrictions.eq("status", AfspraakStatus.UITGEVOERD), 
						Restrictions.isNotNull("conclusie.type"),
						Restrictions.ne("conclusie.type", ColonConclusieType.ON_HOLD))));
			criteria.add(Restrictions.isNull("persoon.overlijdensdatum"));
			criteria.add(Restrictions.or(
				ScreenitRestrictions.getLeeftijdsgrensRestrictions(null, zoekFilter.getMaxLeeftijd(), vandaag),
				Restrictions.not(
					ColonRestrictions.critRondeZonderVerslagNaVerlopenOngunstigeUitslag(DateUtil.toUtilDate(vandaag.minusDays(zoekFilter.getInterval())), "testen", "colonScreeningRonde")
				),
				Restrictions.isNotNull("openUitnodiging.id")
			));
			criteria.add(Restrictions.isNull("nieuweAfspraak"));
		}
		else
		{

			Conjunction critZonderConclusie = Restrictions.conjunction();
			critZonderConclusie.add(Restrictions.isNull("conclusie.type"));
			if (totEnMet != null && totEnMet.isBefore(vandaag))
			{
				critZonderConclusie.add(Restrictions.lt("startTime", DateUtil.toUtilDate(totEnMet)));
			}
			else
			{
				critZonderConclusie.add(Restrictions.lt("startTime", DateUtil.toUtilDate(vandaag)));
			}

				Conjunction critOnHold = Restrictions.conjunction();
				critOnHold.add(Restrictions.eq("conclusie.type", ColonConclusieType.ON_HOLD));
				if (totEnMet != null)
				{
					critOnHold.add(Restrictions.lt("startTime", DateUtil.toUtilDate(totEnMet)));
				}
				criteria.alias("dossier.volgendeUitnodiging", "volgendeUitnodiging");
				criteria.createAlias("volgendeUitnodiging.interval", "interval");
				criteria.add(Restrictions.gtProperty("volgendeUitnodiging.peildatum", "interval.berekendeReferentieDatum"));
				criteria.add(Restrictions.or(critZonderConclusie, critOnHold));
		}

		if (!AfspraakStatus.UITGEVOERD.equals(zoekFilter.getStatus()))
		{
			criteria.add(Subqueries.propertyNotIn("colonScreeningRonde.id", verslagenCrit));
		}

		if (vanaf != null)
		{
			criteria.add(Restrictions.gt("startTime", DateUtil.toUtilDate(vanaf)));
		}
		if (zoekFilter.getStatus() != null && totEnMet != null)
		{
			criteria.add(Restrictions.lt("startTime", DateUtil.toUtilDate(totEnMet)));
		}
		if (zoekFilter.getConclusieTypeFilter() != null)
		{
			if (ConclusieTypeFilter.GEEN_CONCLUSIE.equals(zoekFilter.getConclusieTypeFilter()))
			{
				criteria.add(Restrictions.isNull("conclusie.type"));
			}
			else if (zoekFilter.getConclusieTypeFilter().getConclusieTypes().length == 1)
			{
				criteria.add(Restrictions.eq("conclusie.type", zoekFilter.getConclusieTypeFilter().getConclusieTypes()[0]));
			}
			else if (zoekFilter.getConclusieTypeFilter().getConclusieTypes().length > 1)
			{
				criteria.add(Restrictions.in("conclusie.type", Arrays.asList(zoekFilter.getConclusieTypeFilter().getConclusieTypes())));
			}
		}
		return criteria;
	}

	@Override
	public long countAfsprakenVoorColoscopiecentrum(WerklijstIntakeFilter zoekFilter, ColoscopieCentrum coloscopieCentrum, LocalDate vandaag)
	{
		final BaseCriteria<? extends ColonIntakeAfspraak> criteria = createCriteria(zoekFilter, coloscopieCentrum, vandaag);
		if (moetNogOpGeboortedatumFilteren(zoekFilter))
		{
			List<ColonIntakeAfspraak> afspraken =  (List<ColonIntakeAfspraak>) criteria.list(getSession());
			filterGeboortedatum(zoekFilter.getGeboortedatum(), afspraken);
			return afspraken.size();
		}
		else
		{
			return criteria.countLong(getSession());
		}
	}

	private boolean filterGeboortedatum(Date geboortedatum, List<ColonIntakeAfspraak> afspraken)
	{
		return afspraken.removeIf(a -> !DateUtil.isGeboortedatumGelijk(DateUtil.toLocalDate(geboortedatum), a.getClient()));
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdateWithFlush(AbstractAppointment appointment)
	{
		this.getSession().saveOrUpdate(appointment);
		this.getSession().flush();
	}

	@Override
	public List<Object> getAfsprakenInIntervals(Kamer location, List<Interval> verwijderdeIntervals)
	{
		Criteria criteria = getSession().createCriteria(Afspraak.class);

		criteria.add(Restrictions.eq("location", location));

		criteria.add(Restrictions.or(Restrictions.eq("status", AfspraakStatus.GEPLAND), Restrictions.eq("status", AfspraakStatus.UITGEVOERD)));

		Disjunction disjunction = Restrictions.disjunction();
		for (Interval interval : verwijderdeIntervals)
		{
			disjunction.add(RangeCriteriaBuilder.closedOpen("startTime", "endTime").overlaps(interval));
		}
		criteria.add(disjunction);
		criteria.addOrder(Order.asc("startTime"));

		ProjectionList projectionList = Projections.projectionList() 
			.add(Projections.property("startTime")) 
			.add(Projections.property("endTime")) 
			.add(Projections.property("id"))
		;
		criteria.setProjection(projectionList);

		return criteria.list();
	}

	@Override
	public RoosterItem getRoosterBlokVoorAfspraak(ColonIntakeAfspraak newAfspraak)
	{
		Criteria criteria = getSession().createCriteria(RoosterItem.class);

		criteria.add(Restrictions.eq("location", newAfspraak.getLocation()));

		Interval interval = new Interval(new DateTime(newAfspraak.getStartTime()), new DateTime(newAfspraak.getEndTime()));
		criteria.add(RangeCriteriaBuilder.closedOpen("startTime", "endTime").overlaps(interval));

		return (RoosterItem) criteria.uniqueResult();
	}

	@Override
	public RoosterItem getVrijRoosterBlokVoorAfspraak(ColonIntakeAfspraak newAfspraak)
	{
		Criteria criteria = getSession().createCriteria(RoosterItem.class);

		criteria.add(Restrictions.eq("location", newAfspraak.getLocation()));
		criteria.add(Restrictions.eq("startTime", newAfspraak.getStartTime()));
		criteria.add(Restrictions.isEmpty("afspraken"));

		criteria.setMaxResults(1);
		return (RoosterItem) criteria.uniqueResult();
	}

	@Override
	public List<AfspraakLocatieWrapper> getAllAfsprakLengtesPerLocatie()
	{
		Criteria criteria = getSession().createCriteria(Afspraak.class);

		criteria.createAlias("location", "location");
		criteria.createAlias("location.coloscopieCentrum", "intakeLocatie");

		criteria.add(Restrictions.or(Restrictions.eq("status", AfspraakStatus.GEPLAND), Restrictions.eq("status", AfspraakStatus.UITGEVOERD)));

		criteria.addOrder(Order.desc("startTime"));

		ProjectionList projectionList = Projections.projectionList() 
			.add(Projections.alias(Projections.property("intakeLocatie.id"), "locatieId")) 
			.add(Projections.alias(Projections.property("startTime"), "startTime")) 
			.add(Projections.alias(Projections.property("endTime"), "endTime")) 
		;
		criteria.setProjection(projectionList);
		criteria.setResultTransformer(Transformers.aliasToBean(AfspraakLocatieWrapper.class));

		return criteria.list();
	}

	@Override
	public Date getLaatsteWijzigingsdatumAfspraak(HibernateObject entity)
	{
		AuditReader reader = AuditReaderFactory.get(getSession());
		AuditQuery query = reader.createQuery().forRevisionsOfEntity(Hibernate.getClass(entity), false, false);
		query.add(AuditEntity.id().eq(entity.getId()));
		query.addProjection(AuditEntity.revisionNumber().max());
		Number lastRevision = (Number) query.getSingleResult();
		return lastRevision == null ? null : reader.getRevisionDate(lastRevision);
	}

	@Override
	public List<Object> getRoosterItemsBezetMetAfspraak(Long roosterItemId, Interval currentViewInterval)
	{
		Criteria criteria = getSession().createCriteria(Afspraak.class, "this");

		criteria.add(Restrictions.or(Restrictions.eq("status", AfspraakStatus.GEPLAND), Restrictions.eq("status", AfspraakStatus.UITGEVOERD)));

		criteria.addOrder(Order.desc("this.startTime"));

		criteria.createAlias("this.roosterItem", "roosterItem");

		DetachedCriteria subcriteria = DetachedCriteria.forClass(RoosterItem.class);
		subcriteria.createAlias("recurrence", "recurrence");
		subcriteria.createAlias("recurrence.appointments", "recurrenceItems", JoinType.LEFT_OUTER_JOIN);
		subcriteria.add(Restrictions.eq("recurrenceItems.id", roosterItemId));
		subcriteria.setProjection(Projections.id());

		criteria.add(RangeCriteriaBuilder.closedOpen("this.startTime", "this.endTime").overlaps(currentViewInterval));
		criteria.add(Restrictions.or(Restrictions.eq("roosterItem.id", roosterItemId), Subqueries.propertyIn("roosterItem.id", subcriteria)));

		ProjectionList projectionList = Projections.projectionList() 
			.add(Projections.property("this.startTime")) 
			.add(Projections.property("this.endTime")) 
			.add(Projections.property("this.id"))
		;
		criteria.setProjection(Projections.distinct(projectionList));
		return criteria.list();
	}

}
