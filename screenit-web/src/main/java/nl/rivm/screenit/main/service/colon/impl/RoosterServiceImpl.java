
package nl.rivm.screenit.main.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import nl.rivm.screenit.dao.colon.IntakelocatieVanTotEnMetFilter;
import nl.rivm.screenit.dao.colon.RoosterDao;
import nl.rivm.screenit.exceptions.HeeftAfsprakenException;
import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.exceptions.TijdBlokOverlapException;
import nl.rivm.screenit.main.model.RecurrenceOption;
import nl.rivm.screenit.main.service.colon.RoosterService;
import nl.rivm.screenit.model.Afspraak;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.RoosterItemListViewWrapper;
import nl.rivm.screenit.model.colon.RoosterItemStatus;
import nl.rivm.screenit.model.colon.RoosterListViewFilter;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.colon.planning.ColonBlokkade;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.colon.AfspraakService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.planning.model.IAppointment;
import nl.topicuszorg.planning.model.IRecurrentAppointment;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.AbstractRecurrence;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.NoRecurrence;
import nl.topicuszorg.wicket.planning.services.RecurrenceService;
import nl.topicuszorg.wicket.planning.util.Periode;

import org.apache.commons.collections.CollectionUtils;
import org.joda.time.DateTime;
import org.joda.time.Interval;
import org.joda.time.LocalTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class RoosterServiceImpl implements RoosterService
{

	@Autowired
	private RoosterDao roosterDao;

	@Autowired
	private AfspraakService afspraakService;

	@Autowired
	private RecurrenceService recurrenceService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private Integer maxRoosterUitrolInMonths;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	private static void addNieuwTijdslot(AbstractAppointment tijdslot, Interval currentViewInterval, List<Interval> nieuweRoosterBlokken)
	{
		Interval nieuwRoosterBlok = new Interval(new DateTime(tijdslot.getStartTime()).withSecondOfMinute(0).withMillisOfSecond(0),
			new DateTime(tijdslot.getEndTime()).withSecondOfMinute(0).withMillisOfSecond(0));
		if (currentViewInterval.overlaps(nieuwRoosterBlok) && !nieuweRoosterBlokken.contains(nieuwRoosterBlok))
		{
			nieuweRoosterBlokken.add(nieuwRoosterBlok);
		}
	}

	@Override
	@Transactional(readOnly = true, propagation = Propagation.SUPPORTS)
	public List<RoosterItem> getRooster(Periode periode, List<Kamer> kamers)
	{
		return roosterDao.getAppointments(periode, kamers, RoosterItem.class);
	}

	@Override
	public List<ColonBlokkade> getBlokkades(Periode periode, List<Kamer> kamers)
	{
		return roosterDao.getAppointments(periode, kamers, ColonBlokkade.class);
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public List<RoosterItem> getOneindigeRoostersItems()
	{
		return roosterDao.getOneindigeItems();
	}

	@Override
	@Transactional(readOnly = true, propagation = Propagation.SUPPORTS)
	public void magRoosterItemOpslaanVerwijderen(RoosterItem roosteritem, RecurrenceOption recurrenceOption, Date recurrenceEditEnd,
		Date origRecEndDateTime, boolean wijzigen) throws OpslaanVerwijderenTijdBlokException
	{
		List<Interval> teVerwijderenRoosterBlokken = new ArrayList<>();
		DateTime viewStartDateTime = null;
		DateTime viewEndTime = null;

		if (roosteritem.getRecurrence() != null && !NoRecurrence.class.isAssignableFrom(roosteritem.getRecurrence().getClass()))
		{
			if (roosteritem.getId() != null)
			{
				switch (recurrenceOption)
				{
				case WIJZIG_OF_VERWIJDER_ALLEEN_DEZE_AFSPRAAK:
					viewStartDateTime = new DateTime(roosteritem.getStartTime()).withTimeAtStartOfDay().toDateTime();
					viewEndTime = new DateTime(viewStartDateTime).plusDays(1).withTimeAtStartOfDay().toDateTime();
					break;
				case WIJZIG_OF_VERWIJDER_HELE_SERIE:
					viewStartDateTime = currentDateSupplier.getDateTimeMidnight();
					viewEndTime = new DateTime(origRecEndDateTime);
					break;
				case WIJZIG_OF_VERWIJDER_VANAF_HIER:
					viewStartDateTime = new DateTime(roosteritem.getStartTime()).withTimeAtStartOfDay().toDateTime();
					viewEndTime = new DateTime(origRecEndDateTime);
					break;
				case WIJZIG_OF_VERWIJDER_TOT:
					viewStartDateTime = new DateTime(roosteritem.getStartTime()).withTimeAtStartOfDay().toDateTime();
					viewEndTime = new DateTime(recurrenceEditEnd).plusDays(1).withTimeAtStartOfDay().toDateTime();
					break;
				default:
					break;
				}
			}
			else
			{
				viewStartDateTime = new DateTime(roosteritem.getStartTime());
				LocalTime endTime = new LocalTime(roosteritem.getEndTime());
				Date recurenceEndDate = roosteritem.getRecurrence().getEndDate();
				if (recurenceEndDate == null)
				{
					recurenceEndDate = new DateTime().plusMonths(maxRoosterUitrolInMonths).toDate();
				}
				viewEndTime = new DateTime(recurenceEndDate).withTimeAtStartOfDay().toDateTime().plus(endTime.getMillisOfDay());
			}
		}
		else
		{
			if (roosteritem.getId() != null)
			{
				viewStartDateTime = new DateTime(roosteritem.getStartTime()).withTimeAtStartOfDay().toDateTime();
				viewEndTime = new DateTime(viewStartDateTime).plusDays(1).withTimeAtStartOfDay().toDateTime();
			}
			else
			{
				viewStartDateTime = new DateTime(roosteritem.getStartTime());
				viewEndTime = new DateTime(roosteritem.getEndTime());
			}
		}

		Interval currentViewInterval = new Interval(viewStartDateTime.withSecondOfMinute(0).withMillisOfSecond(0), viewEndTime.withSecondOfMinute(0).withMillisOfSecond(0));
		if (roosteritem.getId() != null)
		{

			List<Object> afspraken = afspraakService.getRoosterItemsBezetMetAfspraak(roosteritem.getId(), currentViewInterval);
			if (afspraken.size() > 0)
			{

				if (wijzigen)
				{
					throw new HeeftAfsprakenException("roosteritem.wijzig.heeftafspraken", afspraken);
				}
				else
				{
					throw new HeeftAfsprakenException("roosteritem.verwijder.heeftafspraken", afspraken);
				}
			}
		}

		List<Interval> nieuweRoosterBlokken = getNieuweTijdSloten(roosteritem, currentViewInterval);

		List<Object> overlapteRoosterBlokken = null;
		if (wijzigen || roosteritem.getId() == null) 
		{
			overlapteRoosterBlokken = roosterDao.getRoosterTijden(nieuweRoosterBlokken, roosteritem, currentViewInterval);
		}

		if (roosteritem.getId() == null) 
		{
			if (CollectionUtils.isNotEmpty(overlapteRoosterBlokken))
			{

				throw new TijdBlokOverlapException("roosteritem.heeft.overlap", overlapteRoosterBlokken);
			}
		}
		else if (wijzigen)
		{
			List<Interval> echteOverlapteRoosterBlokken = new ArrayList<>();

			Set<Long> idsVanBestaandeRoosterblokken = new HashSet<>();
			if (roosteritem.getRecurrence() != null && !NoRecurrence.class.isAssignableFrom(roosteritem.getRecurrence().getClass()))
			{
				for (IRecurrentAppointment appointment : roosteritem.getRecurrence().getAppointments())
				{
					idsVanBestaandeRoosterblokken.add(appointment.getId());
				}
			}
			else
			{
				idsVanBestaandeRoosterblokken.add(roosteritem.getId());
			}

			for (Object overlapteRoosterBlokItem : overlapteRoosterBlokken)
			{
				Object[] roosterItemTijden = (Object[]) overlapteRoosterBlokItem;
				DateTime startDateTimeBestaand = new DateTime(roosterItemTijden[0]).withSecondOfMinute(0).withMillisOfSecond(0);
				DateTime endDateTimeBestaand = new DateTime(roosterItemTijden[1]).withSecondOfMinute(0).withMillisOfSecond(0);
				Interval overlapteRoosterBlok = new Interval(startDateTimeBestaand, endDateTimeBestaand);
				Long roosterItemId = (Long) roosterItemTijden[2];

				if (idsVanBestaandeRoosterblokken.contains(roosterItemId))
				{

					Interval teVerwijderRoosterBlok = new Interval(overlapteRoosterBlok);
					for (Interval nieuwRoosterBlok : nieuweRoosterBlokken)
					{
						if (overlapteRoosterBlok.overlaps(nieuwRoosterBlok))
						{
							if (overlapteRoosterBlok.getStart().isBefore(nieuwRoosterBlok.getStart()))
							{
								teVerwijderRoosterBlok = teVerwijderRoosterBlok.withEnd(nieuwRoosterBlok.getStart());
							}
							if (overlapteRoosterBlok.getEnd().isAfter(nieuwRoosterBlok.getEnd()))
							{
								teVerwijderRoosterBlok = teVerwijderRoosterBlok.withStart(nieuwRoosterBlok.getEnd());
							}
							break;
						}
					}

					if (!teVerwijderRoosterBlok.equals(overlapteRoosterBlok))
					{
						teVerwijderenRoosterBlokken.add(teVerwijderRoosterBlok);
					}
				}
				else
				{

					echteOverlapteRoosterBlokken.add(overlapteRoosterBlok);
				}
			}
			if (echteOverlapteRoosterBlokken.size() > 0)
			{

				throw new TijdBlokOverlapException("roosteritem.heeft.overlap", echteOverlapteRoosterBlokken);
			}
		}
		else
		{

			teVerwijderenRoosterBlokken.addAll(nieuweRoosterBlokken);
		}

		if (CollectionUtils.isNotEmpty(teVerwijderenRoosterBlokken))
		{

			List<Object> afspraken = afspraakService.getAfsprakenKamersInIntervals(roosteritem.getLocation(), teVerwijderenRoosterBlokken);
			if (afspraken.size() > 0)
			{

				if (wijzigen)
				{
					throw new HeeftAfsprakenException("roosteritem.wijzig.heeftafspraken", afspraken);
				}
				else
				{
					throw new HeeftAfsprakenException("roosteritem.verwijder.heeftafspraken", afspraken);
				}
			}

		}
	}

	@Override
	@Transactional(readOnly = true, propagation = Propagation.SUPPORTS)
	public void magBlokkadeOpslaanVerwijderen(ColonBlokkade blokkade, RecurrenceOption recurrenceOption, Date recurrenceEditEnd, Date origRecEndDateTime, boolean wijzigen,
		List<Kamer> kamers) throws OpslaanVerwijderenTijdBlokException
	{

		DateTime viewStartDateTime = null;
		DateTime viewEndTime = null;

		if (blokkade.getRecurrence() != null && !NoRecurrence.class.isAssignableFrom(blokkade.getRecurrence().getClass()))
		{
			if (blokkade.getId() != null)
			{
				switch (recurrenceOption)
				{
				case WIJZIG_OF_VERWIJDER_ALLEEN_DEZE_AFSPRAAK:
					viewStartDateTime = new DateTime(blokkade.getStartTime()).withTimeAtStartOfDay().toDateTime();
					viewEndTime = new DateTime(viewStartDateTime).plusDays(1).withTimeAtStartOfDay().toDateTime();
					break;
				case WIJZIG_OF_VERWIJDER_HELE_SERIE:
					viewStartDateTime = currentDateSupplier.getDateTimeMidnight();
					viewEndTime = new DateTime(origRecEndDateTime);
					break;
				case WIJZIG_OF_VERWIJDER_VANAF_HIER:
					viewStartDateTime = new DateTime(blokkade.getStartTime()).withTimeAtStartOfDay().toDateTime();
					viewEndTime = new DateTime(origRecEndDateTime);
					break;
				case WIJZIG_OF_VERWIJDER_TOT:
					viewStartDateTime = new DateTime(blokkade.getStartTime()).withTimeAtStartOfDay().toDateTime();
					viewEndTime = new DateTime(recurrenceEditEnd).plusDays(1).withTimeAtStartOfDay().toDateTime();
					break;
				default:
					break;
				}
			}
			else
			{
				viewStartDateTime = new DateTime(blokkade.getStartTime());
				LocalTime endTime = new LocalTime(blokkade.getEndTime());
				Date recurenceEndDate = blokkade.getRecurrence().getEndDate();
				if (recurenceEndDate == null)
				{
					recurenceEndDate = new DateTime().plusMonths(maxRoosterUitrolInMonths).toDate();
				}
				int millisOfDayEndTime = endTime.getMillisOfDay();
				viewEndTime = new DateTime(recurenceEndDate).withTimeAtStartOfDay().toDateTime();
				if (millisOfDayEndTime == 0)
				{
					viewEndTime = viewEndTime.plusDays(1);
				}
				else
				{
					viewEndTime = viewEndTime.plus(millisOfDayEndTime);
				}
			}
		}
		else
		{
			if (blokkade.getId() != null)
			{
				viewStartDateTime = new DateTime(blokkade.getStartTime()).withTimeAtStartOfDay().toDateTime();
				viewEndTime = new DateTime(viewStartDateTime).plusDays(1).withTimeAtStartOfDay().toDateTime();
			}
			else
			{
				viewStartDateTime = new DateTime(blokkade.getStartTime());
				viewEndTime = new DateTime(blokkade.getEndTime());
			}
		}

		Interval currentViewInterval = new Interval(viewStartDateTime.withSecondOfMinute(0).withMillisOfSecond(0), viewEndTime.withSecondOfMinute(0).withMillisOfSecond(0));
		List<Interval> nieuweBlokkades = getNieuweTijdSloten(blokkade, currentViewInterval);

		List<Object> afspraken = new ArrayList<>();
		for (Kamer kamer : kamers)
		{
			afspraken.addAll(afspraakService.getAfsprakenKamersInIntervals(kamer, nieuweBlokkades));
		}
		if (afspraken.size() > 0)
		{
			if (blokkade.getId() == null)
			{
				throw new HeeftAfsprakenException("blokkade.nieuw.heeftafspraken", afspraken);
			}
			else if (wijzigen)
			{
				throw new HeeftAfsprakenException("blokkade.wijzig.heeftafspraken", afspraken);
			}
		}
	}

	private List<Interval> getNieuweTijdSloten(AbstractAppointment tijdslot, Interval currentViewInterval)
	{
		List<Interval> nieuweRoosterBlokken = new ArrayList<>();

		AbstractRecurrence recurrence = tijdslot.getRecurrence();
		if (recurrence != null && !NoRecurrence.class.isAssignableFrom(recurrence.getClass()))
		{
			if (recurrence.getId() != null)
			{
				tijdslot = (AbstractAppointment) tijdslot.transientClone();
				recurrence = recurrence.transientClone();
				tijdslot.setRecurrence(recurrence);
			}
			recurrenceService.bepaalRecurrenceAmount(tijdslot, recurrence);
			recurrence.setFirstAppointment(tijdslot);

			AbstractAppointment next = (AbstractAppointment) recurrence.getFirstOccurrence();
			addNieuwTijdslot(next, currentViewInterval, nieuweRoosterBlokken);
			for (int i = 0; i < recurrence.getRecurrenceAmount(); i++)
			{
				next = (AbstractAppointment) recurrence.getNextAppointment(next);
				addNieuwTijdslot(next, currentViewInterval, nieuweRoosterBlokken);
			}
		}
		else
		{
			addNieuwTijdslot(tijdslot, currentViewInterval, nieuweRoosterBlokken);
		}
		return nieuweRoosterBlokken;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void toevoegenHerhaling(AbstractAppointment appointment)
	{
		hibernateService.saveOrUpdate(appointment);
		hibernateService.saveOrUpdate(appointment.getRecurrence());
		recurrenceService.toevoegenHerhaling(appointment, null);

	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public Iterator<RoosterItemListViewWrapper> getRoosterBlokken(String sortProperty, boolean asc, long first, long count, RoosterListViewFilter filter,
		ColoscopieCentrum intakeLocatie)
	{
		filter = filter.clone();
		refineFilterDates(filter);
		return roosterDao.getRoosterBlokken(sortProperty, asc, first, count, filter, intakeLocatie);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public long getRoosterBlokkenCount(RoosterListViewFilter filter, ColoscopieCentrum intakeLocatie)
	{
		filter = filter.clone();
		refineFilterDates(filter);
		return roosterDao.getRoosterBlokkenCount(filter, intakeLocatie);
	}

	private RoosterListViewFilter refineFilterDates(RoosterListViewFilter filter)
	{
		filter.setEndDatum(DateUtil.toUtilDate(DateUtil.toLocalDate(filter.getEndDatum()).plusDays(1)));

		if (filter.getStartDatum().after(filter.getEndDatum()))
		{
			filter.setStartDatum((Date) filter.getEndDatum().clone());
		}
		else
		{
			filter.setStartDatum(DateUtil.toUtilDateMidnight(filter.getStartDatum()));
		}
		return filter;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public RoosterItemStatus getRoosterItemStatus(RoosterItem roosterItem)
	{
		RoosterItemStatus roosterItemStatus = RoosterItemStatus.VRIJ_TE_VERPLAATSEN;
		if (roosterDao.getBlokkades(roosterItem.getLocation(), roosterItem.getStartTime(), roosterItem.getEndTime()).size() > 0)
		{
			roosterItemStatus = RoosterItemStatus.BLOKKADE;
		}
		else
		{
			if (Boolean.TRUE.equals(roosterItem.getCapaciteitMeeBepaald()))
			{
				roosterItemStatus = RoosterItemStatus.GEBRUIKT_VOOR_CAPACITEIT;
			}

			for (Afspraak afspraak : roosterItem.getAfspraken())
			{
				if (AfspraakStatus.VOOR_AGENDA.contains(afspraak.getStatus()))
				{
					roosterItemStatus = RoosterItemStatus.INTAKE_GEPLAND;
				}
			}
		}
		return roosterItemStatus;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public Integer getCurrentAantalRoosterBlokken(ColoscopieCentrum intakeLocatie, Interval periode)
	{
		int currentAantalBlokken = 0;
		for (Kamer kamer : intakeLocatie.getKamers())
		{
			if (!Boolean.FALSE.equals(kamer.getActief()))
			{
				List<Object> roosterBlokken = roosterDao.getCurrentRoosterBlokken(kamer, periode);
				DateTime firstDayOfThisYear = currentDateSupplier.getDateTime().dayOfYear().withMinimumValue().withTimeAtStartOfDay();
				List<ColonBlokkade> blokkades = roosterDao.getBlokkades(kamer, firstDayOfThisYear.toDate(), firstDayOfThisYear.plusYears(1).toDate());
				for (Object object : roosterBlokken)
				{
					Object[] roosterBlok = (Object[]) object;
					Interval baseRoosterBlok = new Interval(new DateTime(roosterBlok[0]), new DateTime(roosterBlok[1]));
					List<Interval> correctedRoosterBlokken = new ArrayList<>();
					correctedRoosterBlokken.add(baseRoosterBlok);
					List<Interval> correctedRoosterBlokkenNew;

					for (IAppointment blokkade : blokkades)
					{
						correctedRoosterBlokkenNew = new ArrayList<>();
						for (Interval roosterBlokToCorrect : correctedRoosterBlokken)
						{
							correctedRoosterBlokkenNew
								.addAll(DateUtil.disjunct(roosterBlokToCorrect, new Interval(new DateTime(blokkade.getStartTime()), new DateTime(blokkade.getEndTime()))));
						}
						correctedRoosterBlokken = correctedRoosterBlokkenNew;
					}
					currentAantalBlokken += correctedRoosterBlokken.size();
				}
			}
		}
		return currentAantalBlokken;
	}

	@Override
	public List<Date> getMdlDatums(Client client, IntakelocatieVanTotEnMetFilter intakeVanTotEnMetFilter)
	{
		return roosterDao.getMdlDatums(client, intakeVanTotEnMetFilter);
	}

	@Override
	public List<ColonBlokkade> getBlokkades(String sortProperty, boolean ascending, long first, long count, RoosterListViewFilter filter, ColoscopieCentrum intakelocatie)
	{
		return roosterDao.getBlokkades(sortProperty, ascending, first, count, filter, intakelocatie);
	}

	@Override
	public long getBlokkadesCount(RoosterListViewFilter filter, ColoscopieCentrum intakelocatie)
	{
		return roosterDao.getBlokkadesCount(filter, intakelocatie);
	}
}
