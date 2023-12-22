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

import java.time.temporal.ChronoUnit;
import java.time.temporal.TemporalAdjusters;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;

import nl.rivm.screenit.dao.colon.IntakelocatieVanTotEnMetFilter;
import nl.rivm.screenit.dao.colon.RoosterDao;
import nl.rivm.screenit.exceptions.HeeftAfsprakenException;
import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.exceptions.TijdBlokOverlapException;
import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.main.model.RecurrenceOption;
import nl.rivm.screenit.main.service.colon.RoosterService;
import nl.rivm.screenit.model.Afspraak;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.RoosterItemListViewWrapper;
import nl.rivm.screenit.model.colon.RoosterItemStatus;
import nl.rivm.screenit.model.colon.RoosterListViewFilter;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.colon.planning.ColonBlokkade;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.rivm.screenit.repository.colon.ColonAfspraakSlotRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.colon.AfspraakService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.planning.model.IAppointment;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.AbstractRecurrence;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.MonthlyRecurrence;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.NoRecurrence;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.WeeklyRecurrence;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.YearlyRecurrence;
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

	@Autowired
	private ColonAfspraakSlotRepository afspraakSlotRepository;

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
		var currentViewRange = getCurrentViewRange(roosteritem, recurrenceOption, recurrenceEditEnd, origRecEndDateTime);
		if (roosteritem.getId() != null)
		{

			List<Object> afspraken = afspraakService.getRoosterItemsBezetMetAfspraak(roosteritem.getId(), currentViewRange);
			if (!afspraken.isEmpty())
			{

				if (wijzigen)
				{
					throw new HeeftAfsprakenException("error.afspraakslot.wijzig.heeft.afspraken", afspraken);
				}
				else
				{
					throw new HeeftAfsprakenException("error.afspraakslot.verwijder.heeft.afspraken", afspraken);
				}
			}
		}

		var nieuweRoosterBlokken = getNieuweTijdSloten(roosteritem, currentViewRange);
		var teVerwijderenRoosterBlokken = new ArrayList<Range<Date>>();

		List<Object> overlapteRoosterBlokken = null;
		if (wijzigen || roosteritem.getId() == null) 
		{
			overlapteRoosterBlokken = roosterDao.getRoosterTijden(nieuweRoosterBlokken, roosteritem, currentViewRange);
		}

		if (roosteritem.getId() == null) 
		{
			if (CollectionUtils.isNotEmpty(overlapteRoosterBlokken))
			{

				throw new TijdBlokOverlapException("error.afspraakslot.heeft.overlap", overlapteRoosterBlokken);
			}
		}
		else if (wijzigen)
		{
			var echteOverlapteRoosterBlokken = new ArrayList<Range<Date>>();
			var idsVanBestaandeRoosterblokken = new HashSet<Long>();
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

				throw new TijdBlokOverlapException("error.afspraakslot.heeft.overlap", echteOverlapteRoosterBlokken);
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
					throw new HeeftAfsprakenException("error.afspraakslot.wijzig.heeft.afspraken", afspraken);
				}
				else
				{
					throw new HeeftAfsprakenException("error.afspraakslot.verwijder.heeft.afspraken", afspraken);
				}
			}
		}
	}

	@Override
	@Transactional(readOnly = true, propagation = Propagation.SUPPORTS)
	public void magBlokkadeOpslaanVerwijderen(ColonBlokkade blokkade, RecurrenceOption recurrenceOption, Date recurrenceEditEnd, Date origRecEndDateTime, boolean wijzigen,
		List<Kamer> kamers) throws HeeftAfsprakenException
	{
		var currentViewRange = getCurrentViewRange(blokkade, recurrenceOption, recurrenceEditEnd, origRecEndDateTime);
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
				throw new HeeftAfsprakenException("error.blokkade.nieuw.heeft.afspraken", afspraken);
			}
			else if (wijzigen)
			{
				throw new HeeftAfsprakenException("error.blokkade.wijzig.heeft.afspraken", afspraken);
			}
		}
	}

	@Override
	public Range<Date> getCurrentViewRange(AbstractAppointment tijdslot, RecurrenceOption recurrenceOption, Date recurrenceEditEnd,
		Date origRecEndDateTime)
	{
		Date viewStartDateTime = null;
		Date viewEndTime = null;

		if (tijdslot.getRecurrence() != null && !NoRecurrence.class.isAssignableFrom(tijdslot.getRecurrence().getClass()))
		{
			if (tijdslot.getId() != null)
			{
				switch (recurrenceOption)
				{
				case WIJZIG_OF_VERWIJDER_ALLEEN_DEZE_AFSPRAAK:
					viewStartDateTime = DateUtil.startDag(tijdslot.getStartTime());
					viewEndTime = DateUtil.plusDagen(viewStartDateTime, 1);
					break;
				case WIJZIG_OF_VERWIJDER_HELE_SERIE:
					viewStartDateTime = currentDateSupplier.getDateMidnight();
					viewEndTime = origRecEndDateTime;
					break;
				case WIJZIG_OF_VERWIJDER_VANAF_HIER:
					viewStartDateTime = DateUtil.startDag(tijdslot.getStartTime());
					viewEndTime = origRecEndDateTime;
					break;
				case WIJZIG_OF_VERWIJDER_TOT:
					viewStartDateTime = DateUtil.startDag(tijdslot.getStartTime());
					viewEndTime = DateUtil.plusDagen(DateUtil.startDag(recurrenceEditEnd), 1);
					break;
				default:
					break;
				}
			}
			else
			{
				viewStartDateTime = tijdslot.getStartTime();
				var endTime = DateUtil.toLocalTime(tijdslot.getEndTime());
				Date recurenceEndDate = tijdslot.getRecurrence().getEndDate();
				if (recurenceEndDate == null)
				{
					recurenceEndDate = DateUtil.plusTijdseenheid(currentDateSupplier.getDate(), maxRoosterUitrolInMonths, ChronoUnit.MONTHS);
				}
				viewEndTime = DateUtil.toUtilDate(DateUtil.toLocalDate(recurenceEndDate).atTime(endTime));
			}
		}
		else
		{
			if (tijdslot.getId() != null)
			{
				viewStartDateTime = DateUtil.startDag(tijdslot.getStartTime());
				viewEndTime = DateUtil.plusDagen(viewStartDateTime, 1);
			}
			else
			{
				viewStartDateTime = tijdslot.getStartTime();
				viewEndTime = tijdslot.getEndTime();
			}
		}

		return Range.closed(DateUtil.startMinuut(viewStartDateTime), DateUtil.startMinuut(viewEndTime));
	}

	@Override
	public List<Range<Date>> getNieuweTijdSloten(AbstractAppointment tijdslot, Range<Date> currentViewRange)
	{
		var nieuweRoosterBlokken = new ArrayList<Range<Date>>();

		var recurrence = tijdslot.getRecurrence();
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

			var next = (AbstractAppointment) recurrence.getFirstOccurrence();
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

	private static void addNieuwTijdslot(AbstractAppointment tijdslot, Range<Date> currentViewInterval, List<Range<Date>> nieuweRoosterBlokken)
	{
		var nieuwRoosterBlok = Range.closed(DateUtil.startMinuut(tijdslot.getStartTime()), DateUtil.startMinuut(tijdslot.getEndTime()));
		if (DateUtil.overlaps(currentViewInterval, nieuwRoosterBlok) && !nieuweRoosterBlokken.contains(nieuwRoosterBlok))
		{
			nieuweRoosterBlokken.add(nieuwRoosterBlok);
		}
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

	@Override
	public void valideerTijdslot(AbstractAppointment tijdslot) throws ValidatieException
	{
		var startDate = tijdslot.getStartTime();
		var endDate = tijdslot.getEndTime();
		valideerTijdslotStartTijdVoorEindTijd(startDate, endDate);
		valideerTijdslotStartTijdVeelvoud(startDate);
		valideerTijdslotStartGewijzigdNaarVerleden(tijdslot, startDate);
		valideerTijdslotHerhaling(tijdslot, startDate);
	}

	@Override
	public Optional<RoosterItem> getRoosterItem(Long id)
	{
		return afspraakSlotRepository.findById(id);
	}

	@Override
	public ColoscopieCentrum getIntakelocatieVanInstellingGebruiker(InstellingGebruiker instellingGebruiker)
	{
		var organisatie = instellingGebruiker.getOrganisatie();
		return (ColoscopieCentrum) HibernateHelper.deproxy(organisatie);
	}

	private static void valideerTijdslotStartTijdVoorEindTijd(Date startTijd, Date eindTijd) throws ValidatieException
	{
		if (!DateUtil.compareAfter(eindTijd, startTijd))
		{
			throw new ValidatieException("error.eind.voor.start");
		}
	}

	private static void valideerTijdslotStartTijdVeelvoud(Date startTijd) throws ValidatieException
	{
		if (DateUtil.toLocalTime(startTijd).getMinute() % 5 != 0)
		{
			throw new ValidatieException("error.minuten.veelvoud.vijf");
		}
	}

	private void valideerTijdslotStartGewijzigdNaarVerleden(AbstractAppointment tijdslot, Date startTijd) throws ValidatieException
	{
		if (tijdslot.getId() != null && DateUtil.compareBefore(startTijd, currentDateSupplier.getDate()))
		{
			throw new ValidatieException("error.start.in.verleden");
		}
		else if (tijdslot.getId() == null && startTijd.before(currentDateSupplier.getDate()))
		{
			throw new ValidatieException("error.nieuwe.start.in.verleden");
		}
	}

	private void valideerTijdslotHerhaling(AbstractAppointment tijdslot, Date startDate) throws ValidatieException
	{
		var recurrence = tijdslot.getRecurrence();
		if (recurrence != null && !NoRecurrence.class.isAssignableFrom(recurrence.getClass()))
		{
			valideerTijdslotRecurrenceEindDatum(recurrence, startDate);
			if (WeeklyRecurrence.class.isAssignableFrom(recurrence.getClass()))
			{
				valideerTijdslotWekelijkseHerhaling(recurrence);
			}
			else if (MonthlyRecurrence.class.isAssignableFrom(recurrence.getClass()))
			{
				valideerTijdslotMaandelijkseHerhaling(recurrence);
			}
			else if (YearlyRecurrence.class.isAssignableFrom(recurrence.getClass()))
			{
				valideerTijdslotJaarlijkseHerhaling(recurrence);
			}
		}
	}

	private void valideerTijdslotRecurrenceEindDatum(AbstractRecurrence recurrence, Date startDatum) throws ValidatieException
	{
		var endDate = recurrence.getEndDate();
		if (endDate != null)
		{
			endDate = DateUtil.toUtilDate(DateUtil.toLocalDateTime(endDate));
			if (startDatum.after(endDate))
			{
				throw new ValidatieException("error.eind.herhaling.moet.na.start");
			}
		}
	}

	private void valideerTijdslotWekelijkseHerhaling(AbstractRecurrence recurrence) throws ValidatieException
	{
		var weeklyRecurrence = (WeeklyRecurrence) HibernateHelper.deproxy(recurrence);
		if (CollectionUtils.isEmpty(weeklyRecurrence.getDagen()))
		{
			throw new ValidatieException("error.geen.dag.voor.herhaling");
		}
		if (weeklyRecurrence.getRecurrenceInterval() == null || weeklyRecurrence.getRecurrenceInterval() < 1)
		{
			throw new ValidatieException("error.keert.weekend.terug.moet.groter.dan.nul");
		}
	}

	private void valideerTijdslotMaandelijkseHerhaling(AbstractRecurrence recurrence) throws ValidatieException
	{
		var monthlyRecurrence = (MonthlyRecurrence) HibernateHelper.deproxy(recurrence);
		if (monthlyRecurrence.getRecurrenceInterval() == null || monthlyRecurrence.getRecurrenceInterval() < 1)
		{
			throw new ValidatieException("error.keert.maand.terug.moet.groter.dan.nul");
		}
		if (monthlyRecurrence.getXthWeekDay() == null || monthlyRecurrence.getDay() == null)
		{
			throw new ValidatieException("error.op.de.elke.maand.verplicht");
		}
	}

	private void valideerTijdslotJaarlijkseHerhaling(AbstractRecurrence recurrence) throws ValidatieException
	{
		var yearlyRecurrence = (YearlyRecurrence) HibernateHelper.deproxy(recurrence);
		if (yearlyRecurrence.getRecurrenceInterval() == null || yearlyRecurrence.getRecurrenceInterval() < 1)
		{
			throw new ValidatieException("error.keert.jaar.terug.moet.groter.dan.nul");
		}
	}
}
