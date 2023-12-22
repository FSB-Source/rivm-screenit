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

import java.time.DayOfWeek;
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
import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.main.service.colon.ColonAfspraakSlotService;
import nl.rivm.screenit.main.service.colon.RoosterService;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.RoosterItemStatus;
import nl.rivm.screenit.model.colon.dto.ColonAfspraakSlotDto;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
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
public class ColonAfspraakSlotServiceImpl implements ColonAfspraakSlotService
{
	private final RoosterService roosterService;

	private final HibernateService hibernateService;

	private final ScheduleService scheduleService;

	private final LogService logService;

	@Override
	@Transactional
	public void createAfspraakSlot(ColonAfspraakSlotDto afspraakSlotDto, InstellingGebruiker instellingGebruiker)
		throws ValidatieException, OpslaanVerwijderenTijdBlokException
	{
		var intakelocatie = roosterService.getIntakelocatieVanInstellingGebruiker(instellingGebruiker);
		var dbRoosterItem = new RoosterItem();
		var roosterItem = converteerAfspraakSlot(afspraakSlotDto, intakelocatie, dbRoosterItem);

		valideerTijdslot(roosterItem, intakelocatie);
		logAction(roosterItem, afspraakSlotDto.getAantalBlokken(), instellingGebruiker, intakelocatie, null, LogGebeurtenis.ROOSTERBLOK_NIEUW);

		var transformedAfspraakSloten = splitAfspraakSlot(roosterItem, afspraakSlotDto.getAantalBlokken(), intakelocatie);
		for (var transformedAfspraakSlot : transformedAfspraakSloten)
		{
			hibernateService.saveOrUpdate(transformedAfspraakSlot);
		}
	}

	@Override
	@Transactional
	public void updateAfspraakSlot(Long id, ColonAfspraakSlotDto afspraakSlotDto, InstellingGebruiker instellingGebruiker)
		throws ValidatieException, OpslaanVerwijderenTijdBlokException, IllegalStateException
	{
		var intakelocatie = roosterService.getIntakelocatieVanInstellingGebruiker(instellingGebruiker);
		var dbRoosterItem = roosterService.getRoosterItem(id).orElseThrow(() -> new IllegalStateException("RoosterItem kan niet worden gevonden"));

		var originalRoosterItem = dbRoosterItem.transientClone();
		var validateRoosterItem = dbRoosterItem.transientClone();
		validateRoosterItem.setId(id);
		hibernateService.getHibernateSession().detach(validateRoosterItem);
		converteerAfspraakSlot(afspraakSlotDto, intakelocatie, validateRoosterItem);
		valideerTijdslot(validateRoosterItem, intakelocatie);

		converteerAfspraakSlot(afspraakSlotDto, intakelocatie, dbRoosterItem);
		logAction(dbRoosterItem, afspraakSlotDto.getAantalBlokken(), instellingGebruiker, intakelocatie, originalRoosterItem, LogGebeurtenis.ROOSTERBLOK_WIJZIG);
		hibernateService.saveOrUpdate(dbRoosterItem);
	}

	@Override
	@Transactional
	public void deleteAfspraakSlot(Long id, InstellingGebruiker instellingGebruiker)
		throws OpslaanVerwijderenTijdBlokException, ValidatieException
	{
		var intakelocatie = roosterService.getIntakelocatieVanInstellingGebruiker(instellingGebruiker);
		var dbRoosterItem = roosterService.getRoosterItem(id).orElseThrow(() -> new ValidatieException("Afspraakslot niet gevonden"));
		var roosterItemStatus = roosterService.getRoosterItemStatus(dbRoosterItem);
		roosterService.magRoosterItemOpslaanVerwijderen(dbRoosterItem, null, null, null, true);

		if (roosterItemStatus == RoosterItemStatus.GEBRUIKT_VOOR_CAPACITEIT)
		{
			throw new ValidatieException("error.roosterblok.gebruikt.voor.capaciteit");
		}

		hibernateService.delete(dbRoosterItem);
		logAction(dbRoosterItem, 1, instellingGebruiker, intakelocatie, dbRoosterItem, LogGebeurtenis.ROOSTERBLOK_VERWIJDEREN);
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
	public void checkCapaciteitBerekening(RoosterItem roosterItem, ColoscopieCentrum intakelocatie) throws ValidatieException
	{
		var roosterItemStatus = roosterService.getRoosterItemStatus(roosterItem);
		if (roosterItemStatus != RoosterItemStatus.GEBRUIKT_VOOR_CAPACITEIT || roosterItem.getId() == null)
		{
			return;
		}

		var startTime = DateUtil.toLocalDateTime(roosterItem.getStartTime());
		var origRoosterItem = EntityAuditUtil.getPreviousVersionOfEntity(roosterItem, hibernateService.getHibernateSession());
		if (origRoosterItem == null)
		{
			return;
		}

		var origStartTime = DateUtil.toLocalDate(origRoosterItem.getStartTime());
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

	private void valideerTijdslot(RoosterItem roosterItem, ColoscopieCentrum intakelocatie) throws ValidatieException, OpslaanVerwijderenTijdBlokException
	{
		roosterService.valideerTijdslot(roosterItem);

		roosterService.magRoosterItemOpslaanVerwijderen(roosterItem, null, null, null, true);

		checkCapaciteitBerekening(roosterItem, intakelocatie);
		checkEindTijdOpZelfdeDag(DateUtil.toLocalDateTime(roosterItem.getStartTime()), DateUtil.toLocalDateTime(roosterItem.getEndTime()), intakelocatie);
	}

	private RoosterItem converteerAfspraakSlot(ColonAfspraakSlotDto afspraakSlotDto, ColoscopieCentrum intakelocatie, RoosterItem roosterItem)
	{
		roosterItem.setStartTime(DateUtil.toUtilDate(afspraakSlotDto.getStartTime()));
		roosterItem.setEndTime(DateUtil.toUtilDate(afspraakSlotDto.getEndTime()));

		var kamer = intakelocatie.getKamers().stream().filter(k -> k.getId().equals(afspraakSlotDto.getKamerId())).findFirst()
			.orElse(null);
		roosterItem.setLocation(kamer);

		if (roosterItem.getId() == null)
		{
			var alleScheduleSets = scheduleService.getAlleRoosterblokken(null);
			var scheduleSet = alleScheduleSets.get(0);
			roosterItem.setScheduleSet(scheduleSet);
			roosterItem.setTitle(scheduleSet.getTitle());
		}

		return roosterItem;
	}

	private static Integer getDuurAfspraakInMinuten(ColoscopieCentrum intakelocatie)
	{
		return intakelocatie.getAfspraakDefinities().get(0).getDuurAfspraakInMinuten();
	}

	@Override
	public List<RoosterItem> splitAfspraakSlot(RoosterItem unsavedObject, Integer aantalBlokken, ColoscopieCentrum intakelocatie)
	{
		var duurAfspraakInMinuten = getDuurAfspraakInMinuten(intakelocatie);
		var roosterItems = new ArrayList<RoosterItem>();
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
			roosterItems.add(splittedRoosterBlok);
		}
		return roosterItems;
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

	private void logActionKamer(RoosterItem unsavedObject, InstellingGebruiker instellingGebruiker, Kamer kamer, @Nullable RoosterItem originalRoosterItem,
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
		else if (gebeurtenis == LogGebeurtenis.ROOSTERBLOK_WIJZIG && originalRoosterItem != null)
		{
			var origStartTime = originalRoosterItem.getStartTime();
			melding = dateTimeFormat.format(origStartTime) + " -> " + melding;

		}
		logService.logGebeurtenis(gebeurtenis, instellingGebruiker, melding, Bevolkingsonderzoek.COLON);
	}

	private static List<Kamer> getActieveKamers(ColoscopieCentrum intakelocatie)
	{
		return intakelocatie.getKamers().stream().filter(Kamer::getActief).collect(Collectors.toList());
	}
}
