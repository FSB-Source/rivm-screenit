package nl.rivm.screenit.main.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.math.BigInteger;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.temporal.TemporalAdjusters;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import nl.rivm.screenit.dao.colon.RoosterDao;
import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.main.service.colon.RoosterService;
import nl.rivm.screenit.model.Afspraak;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.colon.ColonHerhalingsfrequentie;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.RoosterItemListViewWrapper;
import nl.rivm.screenit.model.colon.RoosterItemStatus;
import nl.rivm.screenit.model.colon.RoosterListViewFilter;
import nl.rivm.screenit.model.colon.dto.ColonHerhalingDto;
import nl.rivm.screenit.model.colon.dto.ColonTijdslotDto;
import nl.rivm.screenit.model.colon.enums.ColonTijdSlotType;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.colon.planning.ColonBlokkade;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.rivm.screenit.repository.colon.ColonAfspraakslotRepository;
import nl.rivm.screenit.repository.colon.ColonTijdslotRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.planning.model.IAppointment;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment_;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.AbstractRecurrence;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.MonthlyRecurrence;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.NoRecurrence;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.WeeklyRecurrence;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.YearlyRecurrence;
import nl.topicuszorg.wicket.planning.util.Periode;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.colon.ColonAfspraakslotSpecification.heeftId;
import static nl.rivm.screenit.specification.colon.ColonAfspraakslotSpecification.heeftKamer;
import static nl.rivm.screenit.specification.colon.ColonAfspraakslotSpecification.valtBinnenDatumRange;
import static nl.rivm.screenit.specification.colon.ColonAfspraakslotSpecification.valtBinnenDatumTijdRange;

@Service
public class RoosterServiceImpl implements RoosterService
{
	@Autowired
	private RoosterDao roosterDao;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private ColonAfspraakslotRepository afspraakslotRepository;

	@Autowired
	private ColonTijdslotRepository tijdslotRepository;

	@Override
	public List<ColonBlokkade> getBlokkades(Periode periode, List<Kamer> kamers)
	{
		return roosterDao.getAppointments(periode, kamers, ColonBlokkade.class);
	}

	@Override
	@Transactional
	public List<RoosterItemListViewWrapper> getAlleRoosterBlokkenInPeriode(String sortProperty, boolean asc, RoosterListViewFilter filter, ColoscopieCentrum intakeLocatie)
	{
		filter = filter.clone();
		refineFilterDates(filter);
		return roosterDao.getAlleRoosterBlokkenInPeriode(sortProperty, asc, filter, intakeLocatie);
	}

	@Override
	public List<ColonTijdslotDto> searchTijdslots(RoosterListViewFilter filter, long intakelocatieId, ColonTijdSlotType typeTijdslot)
	{
		refineFilterDates(filter);
		return tijdslotRepository.searchTijdslots(intakelocatieId, DateUtil.toLocalDateTime(filter.getStartDatum()), DateUtil.toLocalDateTime(filter.getEindDatum()),
				filter.getStartTijd(), filter.getEindTijd(), filter.getKamerId(),
				filter.getSqlDagen(), typeTijdslot.getTitle()).stream()
			.map(tuple -> new ColonTijdslotDto(((BigInteger) tuple.get("tijdslotId")).longValue(), DateUtil.toLocalDateTime((Date) tuple.get("startDatum")),
				DateUtil.toLocalDateTime((Date) tuple.get("eindDatum")), (String) tuple.get("kamer"),
				((BigInteger) tuple.get("kamerId")).longValue(), null))
			.collect(Collectors.toList());
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
		filter.setEindDatum(DateUtil.toUtilDate(DateUtil.toLocalDate(filter.getEindDatum()).plusDays(1)));

		if (filter.getStartDatum().after(filter.getEindDatum()))
		{
			filter.setStartDatum((Date) filter.getEindDatum().clone());
		}
		else
		{
			filter.setStartDatum(DateUtil.toUtilDateMidnight(filter.getStartDatum()));
		}
	}

	@Override
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
					break;
				}
			}
		}
		return roosterItemStatus;
	}

	@Override
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
	public List<RoosterItem> getAfspraakslotsInRangesEnKamer(Range<LocalDateTime> range, RoosterItem afspraakslot)
	{
		return afspraakslotRepository.findAll(heeftKamer(afspraakslot.getLocation())
				.and(valtBinnenDatumTijdRange(range).or(heeftId(afspraakslot.getId()))),
			Sort.by(Sort.Direction.ASC, AbstractAppointment_.START_TIME));
	}

	@Override
	public List<RoosterItem> getAfspraakslotsInRange(Range<LocalDate> range)
	{
		return afspraakslotRepository.findAll(valtBinnenDatumRange(range), Sort.by(Sort.Direction.ASC, AbstractAppointment_.START_TIME));
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
		return afspraakslotRepository.findById(id);
	}

	@Override
	public ColoscopieCentrum getIntakelocatieVanInstellingGebruiker(InstellingGebruiker instellingGebruiker)
	{
		var organisatie = instellingGebruiker.getOrganisatie();
		return (ColoscopieCentrum) HibernateHelper.deproxy(organisatie);
	}

	@Override
	public Range<LocalDateTime> getCurrentViewRange(ColonTijdslotDto tijdslot)
	{
		var startDatumTijd = tijdslot.getStartTime();
		var eindDatumTijd = tijdslot.getEndTime();
		if (tijdslot.getHerhaling() != null && !tijdslot.getHerhaling().getFrequentie().equals(ColonHerhalingsfrequentie.GEEN_HERHALING))
		{
			eindDatumTijd = tijdslot.getHerhaling().getEindDatum().plusDays(1).atStartOfDay();
		}
		return Range.open(startDatumTijd, eindDatumTijd);
	}

	@Override
	public <S extends AbstractAppointment> List<S> maakHerhalingTijdslotsAan(S tijdslot, ColonHerhalingDto herhalingDto)
	{
		S tijdslotCone;
		var vorigeTijdslot = tijdslot;
		var herhalingTijdslots = new ArrayList<S>();
		var herhalingEindDatum = DateUtil.eindDag(DateUtil.toUtilDate(herhalingDto.getEindDatum()));
		while (vorigeTijdslot.getStartTime().before(herhalingEindDatum))
		{
			tijdslotCone = getVolgendeTijdslot(vorigeTijdslot, herhalingDto);
			if (tijdslotCone.getStartTime().before(herhalingEindDatum))
			{
				herhalingTijdslots.add(tijdslotCone);
			}
			vorigeTijdslot = tijdslotCone;
		}
		return herhalingTijdslots;
	}

	private <S extends AbstractAppointment> S getVolgendeTijdslot(S tijdslot, ColonHerhalingDto herhalingDto)
	{
		var tijdslotClone = (S) tijdslot.transientClone();

		var volgendeStartTijd = getStartTijdVolgendeTijdslot(DateUtil.toLocalDateTime(tijdslotClone.getStartTime()), herhalingDto);
		var volgendeEindTijd = getEindTijdVolgendeTijdslot(tijdslotClone, volgendeStartTijd);

		tijdslotClone.setStartTime(volgendeStartTijd);
		tijdslotClone.setEndTime(volgendeEindTijd);
		tijdslotClone.setRecurrence(null);
		return tijdslotClone;
	}

	private Date getStartTijdVolgendeTijdslot(LocalDateTime huidigeTijdslotStartTijd, ColonHerhalingDto herhalingDto)
	{
		if (herhalingDto.getFrequentie() == ColonHerhalingsfrequentie.DAGELIJKS)
		{
			return getStartTijdVolgendeDagelijkseTijdslot(huidigeTijdslotStartTijd, herhalingDto.isAlleenWerkdagen());
		}
		var interval = herhalingDto.getFrequentie() == ColonHerhalingsfrequentie.TWEE_WEKELIJKS ? 2 : 1;
		return getStartTijdVolgendeWekelijkseTijdslot(huidigeTijdslotStartTijd, interval, herhalingDto.getDagen());
	}

	private <S extends AbstractAppointment> Date getEindTijdVolgendeTijdslot(AbstractAppointment afspraakslot, Date volgendeStartTijd)
	{
		long tijdSlotDuurInMillis = afspraakslot.getEndTime().getTime() - afspraakslot.getStartTime().getTime();
		long volgendeStartTijdInMillis = volgendeStartTijd.getTime() + tijdSlotDuurInMillis;
		return new Date(volgendeStartTijdInMillis);
	}

	private Date getStartTijdVolgendeDagelijkseTijdslot(LocalDateTime huidigeTijdslotStartTijd, Boolean businessDaysOnly)
	{
		huidigeTijdslotStartTijd = huidigeTijdslotStartTijd.plusDays(1);

		var weekend = Arrays.asList(DayOfWeek.SATURDAY, DayOfWeek.SUNDAY);

		while (Boolean.TRUE.equals(businessDaysOnly) && weekend.contains(huidigeTijdslotStartTijd.getDayOfWeek()))
		{
			huidigeTijdslotStartTijd = huidigeTijdslotStartTijd.plusDays(1);
		}
		return DateUtil.toUtilDate(huidigeTijdslotStartTijd);
	}

	private Date getStartTijdVolgendeWekelijkseTijdslot(LocalDateTime huidigeTijdslotStartTijd, int perAantalWeken, List<Integer> weekdagen)
	{
		for (var weekDag : weekdagen)
		{
			if (weekDag > huidigeTijdslotStartTijd.getDayOfWeek().getValue())
			{
				huidigeTijdslotStartTijd = huidigeTijdslotStartTijd.with(DayOfWeek.of(weekDag));
				return DateUtil.toUtilDate(huidigeTijdslotStartTijd);
			}
		}
		if (!weekdagen.isEmpty())
		{
			huidigeTijdslotStartTijd = huidigeTijdslotStartTijd.with(DayOfWeek.of(weekdagen.get(0)));
		}
		huidigeTijdslotStartTijd = huidigeTijdslotStartTijd.plusWeeks(perAantalWeken);
		return DateUtil.toUtilDate(huidigeTijdslotStartTijd);
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
