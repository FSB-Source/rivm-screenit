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

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.time.temporal.TemporalAdjusters;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import javax.annotation.Nullable;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.main.exception.BeperkingException;
import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.main.service.colon.ColonAfspraakslotService;
import nl.rivm.screenit.main.service.colon.RoosterService;
import nl.rivm.screenit.mappers.colon.ColonAfspraakslotMapper;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.RoosterItemStatus;
import nl.rivm.screenit.model.colon.RoosterListViewFilter;
import nl.rivm.screenit.model.colon.dto.ColonAfspraakslotDto;
import nl.rivm.screenit.model.colon.enums.ColonRoosterBeperking;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.repository.colon.ColonAfspraakslotRepository;
import nl.rivm.screenit.repository.colon.ColonRoosterItemRepository;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.NoRecurrence;
import nl.topicuszorg.wicket.planning.services.ScheduleService;

import org.hibernate.Hibernate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.Range;

@Service
@Slf4j
@AllArgsConstructor
public class ColonAfspraakslotServiceImpl implements ColonAfspraakslotService
{
	private final RoosterService roosterService;

	private final HibernateService hibernateService;

	private final ScheduleService scheduleService;

	private final LogService logService;

	private final ColonAfspraakslotRepository afspraakslotRepository;

	private final ColonAfspraakslotMapper afspraakslotMapper;

	private final ColonRoosterItemRepository roosterItemRepository;

	@Override
	@Transactional
	public void createAfspraakslot(ColonAfspraakslotDto afspraakslotDto, InstellingGebruiker instellingGebruiker)
		throws ValidatieException, OpslaanVerwijderenTijdBlokException, BeperkingException
	{
		var intakelocatie = roosterService.getIntakelocatieVanInstellingGebruiker(instellingGebruiker);
		var dbAfspraakslot = new RoosterItem();
		var afspraakslot = converteerAfspraakslot(afspraakslotDto, intakelocatie, dbAfspraakslot);
		var alleenValidatie = afspraakslotDto.isAlleenValidatie();

		valideerTijdslot(afspraakslot, intakelocatie, !alleenValidatie);
		if (!alleenValidatie)
		{
			logAction(afspraakslot, afspraakslotDto.getAantalBlokken(), instellingGebruiker, intakelocatie, null, LogGebeurtenis.ROOSTERBLOK_NIEUW);

			var transformedAfspraakslots = splitAfspraakslot(afspraakslot, afspraakslotDto.getAantalBlokken(), intakelocatie);
			roosterItemRepository.saveAll(transformedAfspraakslots);
		}
	}

	@Override
	@Transactional
	public void updateAfspraakslot(Long id, ColonAfspraakslotDto afspraakslotDto, InstellingGebruiker instellingGebruiker)
		throws ValidatieException, OpslaanVerwijderenTijdBlokException, IllegalStateException, BeperkingException
	{
		var intakelocatie = roosterService.getIntakelocatieVanInstellingGebruiker(instellingGebruiker);
		var dbAfspraakslot = roosterService.getRoosterItem(id).orElseThrow(() -> new IllegalStateException("RoosterItem kan niet worden gevonden"));

		var originalAfspraakslot = dbAfspraakslot.transientClone();
		var validateAfspraakslot = dbAfspraakslot.transientClone();
		var alleenValidatie = afspraakslotDto.isAlleenValidatie();

		validateAfspraakslot.setId(id);
		hibernateService.getHibernateSession().detach(validateAfspraakslot);
		converteerAfspraakslot(afspraakslotDto, intakelocatie, validateAfspraakslot);
		valideerTijdslot(validateAfspraakslot, intakelocatie, !alleenValidatie);

		if (!alleenValidatie)
		{
			converteerAfspraakslot(afspraakslotDto, intakelocatie, dbAfspraakslot);
			logAction(dbAfspraakslot, afspraakslotDto.getAantalBlokken(), instellingGebruiker, intakelocatie, originalAfspraakslot, LogGebeurtenis.ROOSTERBLOK_WIJZIG);
			roosterItemRepository.save(dbAfspraakslot);
		}
	}

	@Override
	@Transactional
	public void deleteAfspraakslot(Long id, InstellingGebruiker instellingGebruiker)
		throws OpslaanVerwijderenTijdBlokException, ValidatieException
	{
		var intakelocatie = roosterService.getIntakelocatieVanInstellingGebruiker(instellingGebruiker);
		var dbAfspraakslot = roosterService.getRoosterItem(id).orElseThrow(() -> new ValidatieException("error.afspraakslot.niet.gevonden"));
		var roosterItemStatus = roosterService.getRoosterItemStatus(dbAfspraakslot);
		roosterService.magRoosterItemOpslaanVerwijderen(dbAfspraakslot, null, null, null, true);

		if (roosterItemStatus == RoosterItemStatus.GEBRUIKT_VOOR_CAPACITEIT)
		{
			throw new ValidatieException("error.roosterblok.gebruikt.voor.capaciteit");
		}

		roosterItemRepository.delete(dbAfspraakslot);
		logAction(dbAfspraakslot, 1, instellingGebruiker, intakelocatie, dbAfspraakslot, LogGebeurtenis.ROOSTERBLOK_VERWIJDEREN);
	}

	@Override
	public void checkEindTijdOpZelfdeDag(LocalDateTime startDateTime, LocalDateTime endDateTime, ColoscopieCentrum intakelocatie) throws ValidatieException
	{
		if (!startDateTime.equals(endDateTime))
		{
			var volgendeNacht = startDateTime.plusDays(1).toLocalDate().atStartOfDay();
			if (volgendeNacht.isBefore(endDateTime))
			{
				int overgeblevenMinutenVanDeDag = (int) ChronoUnit.MINUTES.between(startDateTime, volgendeNacht);
				var duurAfspraakInMinuten = getDuurAfspraakInMinuten(intakelocatie);
				throw new ValidatieException("error.te.veel.blokken", overgeblevenMinutenVanDeDag / duurAfspraakInMinuten);
			}
		}
	}

	@Override
	public void checkCapaciteitBerekening(RoosterItem afspraakslot, ColoscopieCentrum intakelocatie) throws ValidatieException
	{
		var roosterItemStatus = roosterService.getRoosterItemStatus(afspraakslot);
		if (roosterItemStatus != RoosterItemStatus.GEBRUIKT_VOOR_CAPACITEIT || afspraakslot.getId() == null)
		{
			return;
		}

		var startTime = DateUtil.toLocalDateTime(afspraakslot.getStartTime());
		var origAfspraakslot = EntityAuditUtil.getPreviousVersionOfEntity(afspraakslot, hibernateService.getHibernateSession());
		if (origAfspraakslot == null)
		{
			return;
		}

		var origStartTime = DateUtil.toLocalDate(origAfspraakslot.getStartTime());
		var duurAfspraakInMinuten = getDuurAfspraakInMinuten(intakelocatie);
		var newEindDatumGebruiktVoorCapaciteit = origStartTime.plusWeeks(1).with(TemporalAdjusters.previousOrSame(DayOfWeek.MONDAY))
			.atStartOfDay();
		var origStartRange = Range.openClosed(newEindDatumGebruiktVoorCapaciteit.minusWeeks(1), newEindDatumGebruiktVoorCapaciteit);
		var startRange = Range.openClosed(startTime, startTime.plusMinutes(duurAfspraakInMinuten));

		if (!DateUtil.overlapsLocalDateTime(origStartRange, startRange))
		{
			throw new ValidatieException("error.roosterblok.gebruikt.voor.capaciteit");
		}
	}

	private void valideerTijdslot(RoosterItem afspraakslot, ColoscopieCentrum intakelocatie, boolean negeerZachteBeperking)
		throws ValidatieException, OpslaanVerwijderenTijdBlokException, BeperkingException
	{
		roosterService.valideerBeperkingen(afspraakslot, ColonRoosterBeperking.HARD);
		roosterService.valideerTijdslot(afspraakslot);

		roosterService.magRoosterItemOpslaanVerwijderen(afspraakslot, null, null, null, true);

		checkCapaciteitBerekening(afspraakslot, intakelocatie);
		checkEindTijdOpZelfdeDag(DateUtil.toLocalDateTime(afspraakslot.getStartTime()), DateUtil.toLocalDateTime(afspraakslot.getEndTime()), intakelocatie);

		if (!negeerZachteBeperking)
		{
			roosterService.valideerBeperkingen(afspraakslot, ColonRoosterBeperking.ZACHT);
		}
	}

	private RoosterItem converteerAfspraakslot(ColonAfspraakslotDto afspraakslotDto, ColoscopieCentrum intakelocatie, RoosterItem afspraakslot)
	{
		afspraakslot.setStartTime(DateUtil.toUtilDate(afspraakslotDto.getStartTime()));
		afspraakslot.setEndTime(DateUtil.toUtilDate(afspraakslotDto.getEndTime()));

		var kamer = intakelocatie.getKamers().stream().filter(k -> k.getId().equals(afspraakslotDto.getKamerId())).findFirst()
			.orElse(null);
		afspraakslot.setLocation(kamer);

		if (afspraakslot.getId() == null)
		{
			var alleScheduleSets = scheduleService.getAlleRoosterblokken(null);
			var scheduleSet = alleScheduleSets.get(0);
			afspraakslot.setScheduleSet(scheduleSet);
			afspraakslot.setTitle(scheduleSet.getTitle());
		}

		return afspraakslot;
	}

	private static Integer getDuurAfspraakInMinuten(ColoscopieCentrum intakelocatie)
	{
		return intakelocatie.getAfspraakDefinities().get(0).getDuurAfspraakInMinuten();
	}

	@Override
	public List<RoosterItem> splitAfspraakslot(RoosterItem unsavedObject, Integer aantalBlokken, ColoscopieCentrum intakelocatie)
	{
		var duurAfspraakInMinuten = getDuurAfspraakInMinuten(intakelocatie);
		var afspraakslots = new ArrayList<RoosterItem>();
		var startDate = DateUtil.toLocalDateTime(unsavedObject.getStartTime());
		for (int i = 0; i < aantalBlokken; i++)
		{
			var splittedRoosterBlok = unsavedObject.transientClone();
			var recurrence = unsavedObject.getRecurrence();
			if (recurrence != null && !NoRecurrence.class.isAssignableFrom(Hibernate.getClass(recurrence)))
			{
				var clonedRecurrence = recurrence.transientClone();
				clonedRecurrence.setFirstAppointment(splittedRoosterBlok);
				splittedRoosterBlok.setRecurrence(clonedRecurrence);
			}
			splittedRoosterBlok.setStartTime(DateUtil.toUtilDate(startDate));
			var endTime = startDate.plusMinutes(duurAfspraakInMinuten);
			splittedRoosterBlok.setEndTime(DateUtil.toUtilDate(endTime));
			startDate = endTime;
			afspraakslots.add(splittedRoosterBlok);
		}
		return afspraakslots;
	}

	public void logAction(RoosterItem unsavedObject, int aantalBlokken, InstellingGebruiker instellingGebruiker, ColoscopieCentrum intakelocatie,
		@Nullable RoosterItem origineleBlokkade,
		LogGebeurtenis gebeurtenis)
	{
		var selectedKamer = unsavedObject.getLocation();
		if (selectedKamer != null)
		{
			logActionKamer(unsavedObject, instellingGebruiker, selectedKamer, origineleBlokkade, gebeurtenis, aantalBlokken);
		}
		else
		{
			for (Kamer kamer : getActieveKamers(intakelocatie))
			{
				logActionKamer(unsavedObject, instellingGebruiker, kamer, origineleBlokkade, gebeurtenis, aantalBlokken);
			}
		}
	}

	private void logActionKamer(RoosterItem unsavedObject, InstellingGebruiker instellingGebruiker, Kamer kamer, @Nullable RoosterItem originalAfspraakslot,
		LogGebeurtenis gebeurtenis,
		int aantalBlokken)
	{
		var dateTimeFormat = Constants.getDateTimeFormat();
		var dateFormat = Constants.getDateFormat();
		var melding = dateTimeFormat.format(unsavedObject.getStartTime()) + ", " + kamer.getName() + ", " + kamer.getColoscopieCentrum().getNaam();

		if (unsavedObject.getRecurrence() != null && !NoRecurrence.class.isAssignableFrom(unsavedObject.getRecurrence().getClass()))
		{
			melding += ", in " + unsavedObject.getRecurrence().getName() + " reeks ";
			if (unsavedObject.getRecurrence().getEndDate() != null)
			{
				melding += " t/m " + dateFormat.format(unsavedObject.getRecurrence().getEndDate());
			}
		}
		if (gebeurtenis == LogGebeurtenis.ROOSTERBLOK_NIEUW)
		{
			melding = "#" + aantalBlokken + " sloten, " + melding;
		}
		else if (gebeurtenis == LogGebeurtenis.ROOSTERBLOK_WIJZIG && originalAfspraakslot != null)
		{
			var origStartTime = originalAfspraakslot.getStartTime();
			melding = dateTimeFormat.format(origStartTime) + " -> " + melding;

		}
		logService.logGebeurtenis(gebeurtenis, instellingGebruiker, melding, Bevolkingsonderzoek.COLON);
	}

	private static List<Kamer> getActieveKamers(ColoscopieCentrum intakelocatie)
	{
		return intakelocatie.getKamers().stream().filter(Kamer::getActief).collect(Collectors.toList());
	}

	@Override
	public List<ColonAfspraakslotDto> getAfspraakslots(LocalDate startDate, LocalDate endDate, ColoscopieCentrum intakeLocatie)
	{
		var filter = new RoosterListViewFilter();

		filter.setStartDatum(DateUtil.toUtilDate(startDate));
		filter.setEndDatum(DateUtil.toUtilDate(endDate));

		var items = roosterService.getAlleRoosterBlokkenInPeriode("startTime", true, filter, intakeLocatie);
		var afspraakslots = new ArrayList<ColonAfspraakslotDto>();
		for (var item : items)
		{
			var afspraakslot = afspraakslotRepository.findById(item.getRoosterItemId()).orElse(null);
			var roosterItemStatus = roosterService.getRoosterItemStatus(afspraakslot);
			item.setStatus(roosterItemStatus);
			afspraakslots.add(afspraakslotMapper.roosterListItemViewWrapperToColonAfspraakDto(item));
		}
		return afspraakslots;
	}
}
