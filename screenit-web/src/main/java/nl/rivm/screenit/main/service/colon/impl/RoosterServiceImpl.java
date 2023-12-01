package nl.rivm.screenit.main.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.time.temporal.ChronoField;
import java.time.temporal.ChronoUnit;
import java.time.temporal.TemporalAdjusters;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
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
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.AbstractRecurrence;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.NoRecurrence;
import nl.topicuszorg.wicket.planning.services.RecurrenceService;
import nl.topicuszorg.wicket.planning.util.Periode;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.Range;

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

	private static void addNieuwTijdslot(AbstractAppointment tijdslot, Range<Date> currentViewInterval, List<Range<Date>> nieuweRoosterBlokken)
	{
		var nieuwRoosterBlok = Range.closed(DateUtil.startMinuut(tijdslot.getStartTime()), DateUtil.startMinuut(tijdslot.getEndTime()));
		if (DateUtil.overlaps(currentViewInterval, nieuwRoosterBlok) && !nieuweRoosterBlokken.contains(nieuwRoosterBlok))
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
	@Transactional(readOnly = true, propagation = Propagation.SUPPORTS)
	public void magRoosterItemOpslaanVerwijderen(RoosterItem roosteritem, RecurrenceOption recurrenceOption, Date recurrenceEditEnd,
		Date origRecEndDateTime, boolean wijzigen) throws OpslaanVerwijderenTijdBlokException
	{
		List<Range<Date>> teVerwijderenRoosterBlokken = new ArrayList<>();
		Date viewStartDateTime = null;
		Date viewEndTime = null;

		if (roosteritem.getRecurrence() != null && !NoRecurrence.class.isAssignableFrom(roosteritem.getRecurrence().getClass()))
		{
			if (roosteritem.getId() != null)
			{
				switch (recurrenceOption)
				{
				case WIJZIG_OF_VERWIJDER_ALLEEN_DEZE_AFSPRAAK:
					viewStartDateTime = DateUtil.startDag(roosteritem.getStartTime());
					viewEndTime = DateUtil.plusDagen(viewStartDateTime, 1);
					break;
				case WIJZIG_OF_VERWIJDER_HELE_SERIE:
					viewStartDateTime = currentDateSupplier.getDateMidnight();
					viewEndTime = origRecEndDateTime;
					break;
				case WIJZIG_OF_VERWIJDER_VANAF_HIER:
					viewStartDateTime = DateUtil.startDag(roosteritem.getStartTime());
					viewEndTime = origRecEndDateTime;
					break;
				case WIJZIG_OF_VERWIJDER_TOT:
					viewStartDateTime = DateUtil.startDag(roosteritem.getStartTime());
					viewEndTime = DateUtil.plusDagen(DateUtil.startDag(recurrenceEditEnd), 1);
					break;
				default:
					break;
				}
			}
			else
			{
				viewStartDateTime = roosteritem.getStartTime();
				var endTime = DateUtil.toLocalTime(roosteritem.getEndTime());
				Date recurenceEndDate = roosteritem.getRecurrence().getEndDate();
				if (recurenceEndDate == null)
				{
					recurenceEndDate = DateUtil.plusTijdseenheid(currentDateSupplier.getDate(), maxRoosterUitrolInMonths, ChronoUnit.MONTHS);
				}
				viewEndTime = DateUtil.toUtilDate(DateUtil.toLocalDate(recurenceEndDate).atTime(endTime));
			}
		}
		else
		{
			if (roosteritem.getId() != null)
			{
				viewStartDateTime = DateUtil.startDag(roosteritem.getStartTime());
				viewEndTime = DateUtil.plusDagen(viewStartDateTime, 1);
			}
			else
			{
				viewStartDateTime = roosteritem.getStartTime();
				viewEndTime = roosteritem.getEndTime();
			}
		}

		var currentViewRange = Range.closed(DateUtil.startMinuut(viewStartDateTime), DateUtil.startMinuut(viewEndTime));
		if (roosteritem.getId() != null)
		{

			List<Object> afspraken = afspraakService.getRoosterItemsBezetMetAfspraak(roosteritem.getId(), currentViewRange);
			if (!afspraken.isEmpty())
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

		var nieuweRoosterBlokken = getNieuweTijdSloten(roosteritem, currentViewRange);

		List<Object> overlapteRoosterBlokken = null;
		if (wijzigen || roosteritem.getId() == null) 
		{
			overlapteRoosterBlokken = roosterDao.getRoosterTijden(nieuweRoosterBlokken, roosteritem, currentViewRange);
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
			List<Range<Date>> echteOverlapteRoosterBlokken = new ArrayList<>();

			Set<Long> idsVanBestaandeRoosterblokken = new HashSet<>();
			if (roosteritem.getRecurrence() != null && !NoRecurrence.class.isAssignableFrom(roosteritem.getRecurrence().getClass()))
			{
				for (var appointment : roosteritem.getRecurrence().getAppointments())
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
				var startDateTimeBestaand = DateUtil.startMinuut((Date) roosterItemTijden[0]);
				var endDateTimeBestaand = DateUtil.startMinuut((Date) roosterItemTijden[1]);
				var overlapteRoosterBlok = Range.closed(startDateTimeBestaand, endDateTimeBestaand);
				Long roosterItemId = (Long) roosterItemTijden[2];

				if (idsVanBestaandeRoosterblokken.contains(roosterItemId))
				{

					var teVerwijderRoosterBlok = Range.closed(overlapteRoosterBlok.lowerEndpoint(), overlapteRoosterBlok.upperEndpoint());
					for (var nieuwRoosterBlok : nieuweRoosterBlokken)
					{
						if (DateUtil.overlaps(overlapteRoosterBlok, nieuwRoosterBlok))
						{
							if (overlapteRoosterBlok.lowerEndpoint().before(nieuwRoosterBlok.lowerEndpoint()))
							{
								teVerwijderRoosterBlok = Range.closed(teVerwijderRoosterBlok.lowerEndpoint(), nieuwRoosterBlok.lowerEndpoint());
							}
							if (overlapteRoosterBlok.upperEndpoint().after(nieuwRoosterBlok.upperEndpoint()))
							{
								teVerwijderRoosterBlok = Range.closed(nieuwRoosterBlok.upperEndpoint(), teVerwijderRoosterBlok.upperEndpoint());
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
			if (!echteOverlapteRoosterBlokken.isEmpty())
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

			List<Object> afspraken = afspraakService.getAfsprakenKamersInRanges(roosteritem.getLocation(), teVerwijderenRoosterBlokken);
			if (!afspraken.isEmpty())
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
		Date viewStartDateTime = null;
		Date viewEndTime = null;

		if (blokkade.getRecurrence() != null && !NoRecurrence.class.isAssignableFrom(blokkade.getRecurrence().getClass()))
		{
			if (blokkade.getId() != null)
			{
				switch (recurrenceOption)
				{
				case WIJZIG_OF_VERWIJDER_ALLEEN_DEZE_AFSPRAAK:
					viewStartDateTime = DateUtil.startDag(blokkade.getStartTime());
					viewEndTime = DateUtil.plusDagen(viewStartDateTime, 1);
					break;
				case WIJZIG_OF_VERWIJDER_HELE_SERIE:
					viewStartDateTime = currentDateSupplier.getDateMidnight();
					viewEndTime = origRecEndDateTime;
					break;
				case WIJZIG_OF_VERWIJDER_VANAF_HIER:
					viewStartDateTime = DateUtil.startDag(blokkade.getStartTime());
					viewEndTime = origRecEndDateTime;
					break;
				case WIJZIG_OF_VERWIJDER_TOT:
					viewStartDateTime = DateUtil.startDag(blokkade.getStartTime());
					viewEndTime = DateUtil.plusDagen(DateUtil.startDag(recurrenceEditEnd), 1);
					break;
				default:
					break;
				}
			}
			else
			{
				viewStartDateTime = blokkade.getStartTime();
				var endTime = DateUtil.toLocalTime(blokkade.getEndTime());
				var recurenceEndDate = blokkade.getRecurrence().getEndDate();
				if (recurenceEndDate == null)
				{
					recurenceEndDate = DateUtil.plusTijdseenheid(currentDateSupplier.getDate(), maxRoosterUitrolInMonths, ChronoUnit.MONTHS);
				}
				int millisOfDayEndTime = endTime.get(ChronoField.MILLI_OF_DAY);
				viewEndTime = DateUtil.startDag(recurenceEndDate);
				if (millisOfDayEndTime == 0)
				{
					viewEndTime = DateUtil.plusDagen(viewEndTime, 1);
				}
				else
				{
					viewEndTime = DateUtil.plusTijdseenheid(viewEndTime, millisOfDayEndTime, ChronoUnit.MILLIS);
				}
			}
		}
		else
		{
			if (blokkade.getId() != null)
			{
				viewStartDateTime = DateUtil.startDag(blokkade.getStartTime());
				viewEndTime = DateUtil.plusDagen(viewStartDateTime, 1);
			}
			else
			{
				viewStartDateTime = blokkade.getStartTime();
				viewEndTime = blokkade.getEndTime();
			}
		}

		var currentViewRange = Range.closed(DateUtil.startMinuut(viewStartDateTime), DateUtil.startMinuut(viewEndTime));
		var nieuweBlokkades = getNieuweTijdSloten(blokkade, currentViewRange);

		List<Object> afspraken = new ArrayList<>();
		for (Kamer kamer : kamers)
		{
			afspraken.addAll(afspraakService.getAfsprakenKamersInRanges(kamer, nieuweBlokkades));
		}
		if (!afspraken.isEmpty())
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

	private List<Range<Date>> getNieuweTijdSloten(AbstractAppointment tijdslot, Range<Date> currentViewRange)
	{
		List<Range<Date>> nieuweRoosterBlokken = new ArrayList<>();

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
			addNieuwTijdslot(next, currentViewRange, nieuweRoosterBlokken);
			for (int i = 0; i < recurrence.getRecurrenceAmount(); i++)
			{
				next = (AbstractAppointment) recurrence.getNextAppointment(next);
				addNieuwTijdslot(next, currentViewRange, nieuweRoosterBlokken);
			}
		}
		else
		{
			addNieuwTijdslot(tijdslot, currentViewRange, nieuweRoosterBlokken);
		}
		return nieuweRoosterBlokken;
	}

	@Override
	@Transactional
	public void toevoegenHerhaling(AbstractAppointment appointment)
	{
		hibernateService.saveOrUpdate(appointment);
		hibernateService.saveOrUpdate(appointment.getRecurrence());
		recurrenceService.toevoegenHerhaling(appointment, null);

	}

	@Override
	@Transactional
	public List<RoosterItemListViewWrapper> getAlleRoosterBlokkenInPeriode(String sortProperty, boolean asc, RoosterListViewFilter filter,
		ColoscopieCentrum intakeLocatie)
	{
		filter = filter.clone();
		refineFilterDates(filter);
		return roosterDao.getAlleRoosterBlokkenInPeriode(sortProperty, asc, filter, intakeLocatie);
	}

	@Override
	@Transactional
	public List<RoosterItemListViewWrapper> getRoosterBlokken(String sortProperty, boolean asc, long first, long count, RoosterListViewFilter filter,
		ColoscopieCentrum intakeLocatie)
	{
		filter = filter.clone();
		refineFilterDates(filter);
		return roosterDao.getRoosterBlokken(sortProperty, asc, first, count, filter, intakeLocatie);
	}

	@Override
	@Transactional
	public long getRoosterBlokkenCount(RoosterListViewFilter filter, ColoscopieCentrum intakeLocatie)
	{
		filter = filter.clone();
		refineFilterDates(filter);
		return roosterDao.getRoosterBlokkenCount(filter, intakeLocatie);
	}

	private void refineFilterDates(RoosterListViewFilter filter)
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
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public RoosterItemStatus getRoosterItemStatus(RoosterItem roosterItem)
	{
		RoosterItemStatus roosterItemStatus = RoosterItemStatus.VRIJ_TE_VERPLAATSEN;
		if (!roosterDao.getBlokkades(roosterItem.getLocation(), roosterItem.getStartTime(), roosterItem.getEndTime()).isEmpty())
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
	public Integer getCurrentAantalRoosterBlokken(ColoscopieCentrum intakeLocatie, Range<Date> periode)
	{
		int currentAantalBlokken = 0;
		for (Kamer kamer : intakeLocatie.getKamers())
		{
			if (!Boolean.FALSE.equals(kamer.getActief()))
			{
				List<Object> roosterBlokken = roosterDao.getCurrentRoosterBlokken(kamer, periode);
				var firstDayOfThisYear = currentDateSupplier.getLocalDate().with(TemporalAdjusters.firstDayOfYear());
				List<ColonBlokkade> blokkades = roosterDao.getBlokkades(kamer, DateUtil.toUtilDate(firstDayOfThisYear), DateUtil.toUtilDate(firstDayOfThisYear.plusYears(1)));
				for (Object object : roosterBlokken)
				{
					Object[] roosterBlok = (Object[]) object;
					Range<Date> baseRoosterBlok = Range.closed((Date) roosterBlok[0], (Date) roosterBlok[1]);
					List<Range<Date>> correctedRoosterBlokken = new ArrayList<>();
					correctedRoosterBlokken.add(baseRoosterBlok);
					List<Range<Date>> correctedRoosterBlokkenNew;

					for (IAppointment blokkade : blokkades)
					{
						correctedRoosterBlokkenNew = new ArrayList<>();
						for (var roosterBlokToCorrect : correctedRoosterBlokken)
						{
							correctedRoosterBlokkenNew.addAll(DateUtil.disjunct(roosterBlokToCorrect, Range.closed(blokkade.getStartTime(), blokkade.getEndTime())));
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
