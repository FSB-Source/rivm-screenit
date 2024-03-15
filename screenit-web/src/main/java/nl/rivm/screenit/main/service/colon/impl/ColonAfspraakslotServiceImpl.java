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
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import javax.annotation.Nullable;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.exceptions.HeeftAfsprakenException;
import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.exceptions.TijdBlokOverlapException;
import nl.rivm.screenit.main.exception.BeperkingException;
import nl.rivm.screenit.main.exception.BulkAanmakenException;
import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.main.service.colon.ColonAfspraakslotService;
import nl.rivm.screenit.main.service.colon.ColonFeestdagService;
import nl.rivm.screenit.main.service.colon.ColonRoosterBeperkingService;
import nl.rivm.screenit.main.service.colon.RoosterService;
import nl.rivm.screenit.mappers.colon.ColonAfspraakslotMapper;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.colon.ColonHerhalingsfrequentie;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.RoosterItemStatus;
import nl.rivm.screenit.model.colon.RoosterListViewFilter;
import nl.rivm.screenit.model.colon.dto.ColonAfspraakslotDto;
import nl.rivm.screenit.model.colon.dto.ColonAfspraakslotHerhalingDto;
import nl.rivm.screenit.model.colon.dto.ColonRoosterBeperkingenDto;
import nl.rivm.screenit.model.colon.enums.ColonRoosterBeperking;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.repository.colon.ColonAfspraakslotRepository;
import nl.rivm.screenit.repository.colon.ColonRoosterItemRepository;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.colon.AfspraakService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment;
import nl.topicuszorg.wicket.planning.services.ScheduleService;

import org.apache.commons.collections.CollectionUtils;
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

	private final ColonRoosterBeperkingService roosterBeperkingService;

	private final ColonFeestdagService feestdagService;

	private final AfspraakService afspraakService;

	@Override
	@Transactional
	public void createAfspraakslot(ColonAfspraakslotDto afspraakslotDto, InstellingGebruiker instellingGebruiker)
		throws ValidatieException, OpslaanVerwijderenTijdBlokException, BeperkingException, BulkAanmakenException
	{
		var intakelocatie = roosterService.getIntakelocatieVanInstellingGebruiker(instellingGebruiker);
		var dbAfspraakslot = new RoosterItem();
		var afspraakslot = converteerAfspraakslot(afspraakslotDto, intakelocatie, dbAfspraakslot);
		var alleenValidatie = afspraakslotDto.isAlleenValidatie();

		var aanTeMakenAfspraakslots = new ArrayList<RoosterItem>();
		var bulkAanmaken = afspraakslotDto.getHerhaling().getFrequentie() != ColonHerhalingsfrequentie.GEEN_HERHALING;

		aanTeMakenAfspraakslots.add(afspraakslot);

		if (bulkAanmaken)
		{
			aanTeMakenAfspraakslots.addAll(maakHerhalingAfspraakslotsAan(afspraakslot, afspraakslotDto));
			valideerBulkAfspraakslots(aanTeMakenAfspraakslots, intakelocatie, getCurrentViewRange(afspraakslotDto), alleenValidatie);
		}
		else
		{
			valideerAfspraakslot(afspraakslot, intakelocatie, getCurrentViewRange(afspraakslot), !alleenValidatie, false);
		}

		if (!alleenValidatie)
		{
			logAction(afspraakslot, afspraakslotDto.getAantalBlokken(), instellingGebruiker, intakelocatie, null, LogGebeurtenis.ROOSTERBLOK_NIEUW,
				afspraakslotDto.getHerhaling());

			var transformedAfspraakslots = splitAfspraakslots(aanTeMakenAfspraakslots, afspraakslotDto.getAantalBlokken(), intakelocatie);
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
		valideerAfspraakslot(validateAfspraakslot, intakelocatie, getCurrentViewRange(validateAfspraakslot), !alleenValidatie, true);

		if (!alleenValidatie)
		{
			converteerAfspraakslot(afspraakslotDto, intakelocatie, dbAfspraakslot);
			logAction(dbAfspraakslot, afspraakslotDto.getAantalBlokken(), instellingGebruiker, intakelocatie, originalAfspraakslot, LogGebeurtenis.ROOSTERBLOK_WIJZIG, null);
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
		roosterService.magRoosterItemOpslaanVerwijderen(dbAfspraakslot, getCurrentViewRange(dbAfspraakslot), true);

		if (roosterItemStatus == RoosterItemStatus.GEBRUIKT_VOOR_CAPACITEIT)
		{
			throw new ValidatieException("error.roosterblok.gebruikt.voor.capaciteit");
		}

		roosterItemRepository.delete(dbAfspraakslot);
		logAction(dbAfspraakslot, 1, instellingGebruiker, intakelocatie, dbAfspraakslot, LogGebeurtenis.ROOSTERBLOK_VERWIJDEREN, null);
	}

	@Override
	public List<ColonAfspraakslotDto> getAfspraakslots(LocalDate startDate, LocalDate endDate, ColoscopieCentrum intakeLocatie)
	{
		var filter = new RoosterListViewFilter();

		filter.setStartDatum(DateUtil.toUtilDate(startDate));
		filter.setEndDatum(DateUtil.toUtilDate(endDate));

		var gevondenAfspraakslots = roosterService.getAlleRoosterBlokkenInPeriode("startTime", true, filter, intakeLocatie);
		var afspraakslots = new ArrayList<ColonAfspraakslotDto>();
		for (var gevondenAfspraakslot : gevondenAfspraakslots)
		{
			var afspraakslot = afspraakslotRepository.findById(gevondenAfspraakslot.getRoosterItemId()).orElse(null);
			var afspraakslotStatus = roosterService.getRoosterItemStatus(afspraakslot);
			gevondenAfspraakslot.setStatus(afspraakslotStatus);
			afspraakslots.add(afspraakslotMapper.roosterListItemViewWrapperToColonAfspraakDto(gevondenAfspraakslot));
		}
		return afspraakslots;
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
		var afspraakslotStatus = roosterService.getRoosterItemStatus(afspraakslot);
		if (afspraakslotStatus != RoosterItemStatus.GEBRUIKT_VOOR_CAPACITEIT || afspraakslot.getId() == null)
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

	private void valideerAfspraakslot(RoosterItem afspraakslot, ColoscopieCentrum intakelocatie, Range<Date> currentViewRange,
		boolean negeerZachteBeperking, boolean wijzigen)
		throws ValidatieException, OpslaanVerwijderenTijdBlokException, BeperkingException
	{
		valideerBeperkingen(afspraakslot, ColonRoosterBeperking.HARD);
		roosterService.valideerTijdslot(afspraakslot);
		magRoosterItemOpslaanVerwijderen(afspraakslot, List.of(afspraakslot), currentViewRange, wijzigen);

		checkCapaciteitBerekening(afspraakslot, intakelocatie);
		checkEindTijdOpZelfdeDag(DateUtil.toLocalDateTime(afspraakslot.getStartTime()), DateUtil.toLocalDateTime(afspraakslot.getEndTime()), intakelocatie);

		if (!negeerZachteBeperking)
		{
			valideerBeperkingen(afspraakslot, ColonRoosterBeperking.ZACHT);
		}
	}

	private void valideerBulkAfspraakslots(List<RoosterItem> aanTeMakenAfspraakslots, ColoscopieCentrum intakelocatie, Range<Date> currentViewRange, boolean alleenValidatie)
		throws BulkAanmakenException
	{
		var bulkAanmakenException = new BulkAanmakenException();
		var invalideAfspraakslots = new ArrayList<RoosterItem>();
		for (var afspraakslot : aanTeMakenAfspraakslots)
		{
			try
			{
				valideerBeperkingen(afspraakslot, ColonRoosterBeperking.HARD);
				roosterService.valideerTijdslot(afspraakslot);
				magRoosterItemOpslaanVerwijderen(afspraakslot, aanTeMakenAfspraakslots, currentViewRange, true);
				checkCapaciteitBerekening(afspraakslot, intakelocatie);
				checkEindTijdOpZelfdeDag(DateUtil.toLocalDateTime(afspraakslot.getStartTime()), DateUtil.toLocalDateTime(afspraakslot.getEndTime()), intakelocatie);
				valideerBeperkingen(afspraakslot, ColonRoosterBeperking.ZACHT);
			}
			catch (ValidatieException | OpslaanVerwijderenTijdBlokException | BeperkingException ex)
			{
				if (alleenValidatie)
				{
					bulkAanmakenException.addException(afspraakslot, ex);
					invalideAfspraakslots.add(afspraakslot);
				}

				else if (!(ex instanceof BeperkingException && ((BeperkingException) ex).getBeperkingType() == ColonRoosterBeperking.ZACHT))
				{
					invalideAfspraakslots.add(afspraakslot);
				}
			}
		}
		if (!bulkAanmakenException.isEmpty())
		{
			throw bulkAanmakenException;
		}

		aanTeMakenAfspraakslots.removeAll(invalideAfspraakslots);
	}

	public void magRoosterItemOpslaanVerwijderen(RoosterItem afspraakslot, List<RoosterItem> herhalingAfspraakslots, Range<Date> currentViewRange, boolean wijzigen)
		throws OpslaanVerwijderenTijdBlokException
	{
		if (afspraakslot.getId() != null)
		{

			var afspraken = afspraakService.getRoosterItemsBezetMetAfspraak(afspraakslot.getId(), currentViewRange);
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

		var nieuweAfspraakslots = getNieuweTijdsloten(herhalingAfspraakslots);

		List<RoosterItem> overlapteAfspraakslots = null;
		if (wijzigen || afspraakslot.getId() == null) 
		{
			overlapteAfspraakslots = roosterService.getAfspraakslotsInRange(nieuweAfspraakslots, afspraakslot);
		}

		if (afspraakslot.getId() == null) 
		{
			if (CollectionUtils.isNotEmpty(overlapteAfspraakslots))
			{

				throw new TijdBlokOverlapException("error.afspraakslot.heeft.overlap", overlapteAfspraakslots);
			}
		}
		else if (wijzigen)
		{
			var echteOverlapteAfspraakslots = new ArrayList<Range<Date>>();

			for (var overlapteAfspraakslot : overlapteAfspraakslots)
			{
				var startDateTimeBestaand = DateUtil.startMinuut(overlapteAfspraakslot.getStartTime());
				var endDateTimeBestaand = DateUtil.startMinuut(overlapteAfspraakslot.getEndTime());
				var overlapteAfspraakslotRange = Range.closed(startDateTimeBestaand, endDateTimeBestaand);

				if (!afspraakslot.getId().equals(overlapteAfspraakslot.getId()))
				{
					echteOverlapteAfspraakslots.add(overlapteAfspraakslotRange);
				}
			}
			if (!echteOverlapteAfspraakslots.isEmpty())
			{
				throw new TijdBlokOverlapException("error.afspraakslot.heeft.overlap", echteOverlapteAfspraakslots);
			}
		}
	}

	private List<Range<Date>> getNieuweTijdsloten(List<? extends AbstractAppointment> aanTeMakenTijdslots)
	{
		return aanTeMakenTijdslots.stream().map(tijdslot -> Range.closed(DateUtil.startMinuut(tijdslot.getStartTime()), DateUtil.startMinuut(tijdslot.getEndTime()))).collect(
			Collectors.toList());
	}

	@Override
	public void valideerBeperkingen(AbstractAppointment tijdslot, ColonRoosterBeperking beperkingType) throws BeperkingException
	{
		var beperkingen = roosterBeperkingService.getRoosterBeperkingen();
		var exceptie = new BeperkingException();
		exceptie.setBeperkingType(beperkingType);

		valideerNachtBeperkingBegin(tijdslot, beperkingType, exceptie, beperkingen);
		valideerNachtBeperkingEind(tijdslot, beperkingType, exceptie, beperkingen);
		valideerZaterdagBeperking(tijdslot, beperkingType, exceptie, beperkingen);
		valideerZondagBeperking(tijdslot, beperkingType, exceptie, beperkingen);
		valideerFeestdagBeperking(tijdslot, beperkingType, exceptie);

		if (!exceptie.getExceptions().isEmpty())
		{
			throw exceptie;
		}
	}

	private void valideerNachtBeperkingBegin(AbstractAppointment tijdslot, ColonRoosterBeperking beperkingType, BeperkingException exceptie, ColonRoosterBeperkingenDto beperkingen)
	{
		if (beperkingen.getNachtBeperkingType() == beperkingType && beperkingen.getNachtBeperkingBegin().isBefore(DateUtil.toLocalTime(tijdslot.getEndTime())))
		{
			exceptie.addException(new ValidatieException(beperkingType == ColonRoosterBeperking.HARD ? "error.harde.nachtbeperking.begin" : "error.zachte.nachtbeperking.begin",
				beperkingen.getNachtBeperkingBegin().toString()));
		}
	}

	private void valideerNachtBeperkingEind(AbstractAppointment tijdslot, ColonRoosterBeperking beperkingType, BeperkingException exceptie, ColonRoosterBeperkingenDto beperkingen)
	{
		if (beperkingen.getNachtBeperkingType() == beperkingType && beperkingen.getNachtBeperkingEind().isAfter(DateUtil.toLocalTime(tijdslot.getStartTime())))
		{
			exceptie.addException(new ValidatieException(beperkingType == ColonRoosterBeperking.HARD ? "error.harde.nachtbeperking.eind" : "error.zachte.nachtbeperking.eind",
				beperkingen.getNachtBeperkingEind().toString()));
		}
	}

	private void valideerZaterdagBeperking(AbstractAppointment tijdslot, ColonRoosterBeperking beperkingType, BeperkingException exceptie, ColonRoosterBeperkingenDto beperkingen)
	{
		if (beperkingen.getZaterdagBeperkingType() == beperkingType && DateUtil.toLocalDate(tijdslot.getStartTime()).getDayOfWeek() == DayOfWeek.SATURDAY)
		{
			exceptie.addException(
				new ValidatieException(beperkingType == ColonRoosterBeperking.HARD ? "error.harde.weekendbeperking.zaterdag" : "error.zachte.weekendbeperking.zaterdag"));
		}
	}

	private void valideerZondagBeperking(AbstractAppointment tijdslot, ColonRoosterBeperking beperkingType, BeperkingException exceptie, ColonRoosterBeperkingenDto beperkingen)
	{
		if (beperkingen.getZondagBeperkingType() == beperkingType && DateUtil.toLocalDate(tijdslot.getStartTime()).getDayOfWeek() == DayOfWeek.SUNDAY)
		{
			exceptie.addException(
				new ValidatieException(beperkingType == ColonRoosterBeperking.HARD ? "error.harde.weekendbeperking.zondag" : "error.zachte.weekendbeperking.zondag"));
		}
	}

	private void valideerFeestdagBeperking(AbstractAppointment tijdslot, ColonRoosterBeperking beperkingType, BeperkingException exceptie)
	{
		var feestdagen = feestdagService.getFeestdagen();
		var feestdagOpDagTijdslot = feestdagen.stream().filter(feestdag -> DateUtil.isZelfdeDag(tijdslot.getStartTime(), DateUtil.toUtilDate(feestdag.getDatum()))).findFirst()
			.orElse(null);
		if (feestdagOpDagTijdslot != null && feestdagOpDagTijdslot.getBeperking() == beperkingType)
		{
			exceptie.addException(
				new ValidatieException(beperkingType == ColonRoosterBeperking.HARD ? "error.harde.feestdagbeperking" : "error.zachte.feestdagbeperking",
					feestdagOpDagTijdslot.getNaam()));
		}
	}

	private Range<Date> getCurrentViewRange(ColonAfspraakslotDto afspraakslot)
	{
		var startDatumTijd = DateUtil.toUtilDate(afspraakslot.getStartTime());
		var eindDatumTijd = DateUtil.toUtilDate(afspraakslot.getEndTime());
		if (!afspraakslot.getHerhaling().getFrequentie().equals(ColonHerhalingsfrequentie.GEEN_HERHALING))
		{
			eindDatumTijd = DateUtil.toUtilDate(afspraakslot.getHerhaling().getEindDatum());
		}
		return Range.closed(startDatumTijd, eindDatumTijd);
	}

	private Range<Date> getCurrentViewRange(RoosterItem afspraakslot)
	{
		return Range.closed(afspraakslot.getStartTime(), afspraakslot.getEndTime());
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

	private List<RoosterItem> splitAfspraakslots(List<RoosterItem> aanTeMakenAfspraakslots, Integer aantalBlokken, ColoscopieCentrum intakelocatie)
	{
		var afspraakslots = new ArrayList<RoosterItem>();
		var duurAfspraakslotsInMinuten = getDuurAfspraakInMinuten(intakelocatie);
		for (var afspraakslot : aanTeMakenAfspraakslots)
		{
			var startDatumTijd = DateUtil.toLocalDateTime(afspraakslot.getStartTime());

			for (int i = 0; i < aantalBlokken; i++)
			{
				var splittedAfspraakslot = afspraakslot.transientClone();
				splittedAfspraakslot.setStartTime(DateUtil.toUtilDate(startDatumTijd));
				var eindDatumTijd = startDatumTijd.plusMinutes(duurAfspraakslotsInMinuten);
				splittedAfspraakslot.setEndTime(DateUtil.toUtilDate(eindDatumTijd));
				startDatumTijd = eindDatumTijd;
				afspraakslots.add(splittedAfspraakslot);
			}
		}
		return afspraakslots;
	}

	public void logAction(RoosterItem unsavedObject, int aantalBlokken, InstellingGebruiker instellingGebruiker, ColoscopieCentrum intakelocatie,
		@Nullable RoosterItem origineleAfspraakslot, LogGebeurtenis gebeurtenis, ColonAfspraakslotHerhalingDto herhalingDto)
	{
		var selectedKamer = unsavedObject.getLocation();
		if (selectedKamer != null)
		{
			logActionKamer(unsavedObject, instellingGebruiker, selectedKamer, origineleAfspraakslot, gebeurtenis, aantalBlokken, herhalingDto);
		}
		else
		{
			for (Kamer kamer : getActieveKamers(intakelocatie))
			{
				logActionKamer(unsavedObject, instellingGebruiker, kamer, origineleAfspraakslot, gebeurtenis, aantalBlokken, herhalingDto);
			}
		}
	}

	private void logActionKamer(RoosterItem unsavedObject, InstellingGebruiker instellingGebruiker, Kamer kamer, @Nullable RoosterItem originalAfspraakslot,
		LogGebeurtenis gebeurtenis, int aantalBlokken, ColonAfspraakslotHerhalingDto herhalingDto)
	{
		var dateTimeFormat = Constants.getDateTimeFormat();
		var melding = dateTimeFormat.format(unsavedObject.getStartTime()) + ", " + kamer.getName() + ", " + kamer.getColoscopieCentrum().getNaam();

		if (herhalingDto != null && herhalingDto.getFrequentie() != ColonHerhalingsfrequentie.GEEN_HERHALING)
		{
			melding += ", in " + herhalingDto.getFrequentie().getNaam() + " reeks ";
			melding += " t/m " + herhalingDto.getEindDatum().format(DateUtil.LOCAL_DATE_FORMAT);
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

	private List<RoosterItem> maakHerhalingAfspraakslotsAan(RoosterItem afspraakslot, ColonAfspraakslotDto afspraakslotDto)
	{
		RoosterItem afspraakslotCone;
		var vorigeAfspraakslot = afspraakslot;
		var herhalingAfspraakslots = new ArrayList<RoosterItem>();
		var herhalingEindDatum = DateUtil.eindDag(DateUtil.toUtilDate(afspraakslotDto.getHerhaling().getEindDatum()));
		while (vorigeAfspraakslot.getStartTime().before(herhalingEindDatum))
		{
			afspraakslotCone = getVolgendeAfspraak(vorigeAfspraakslot, afspraakslotDto);
			if (afspraakslotCone.getStartTime().before(herhalingEindDatum))
			{
				herhalingAfspraakslots.add(afspraakslotCone);
			}
			vorigeAfspraakslot = afspraakslotCone;
		}
		return herhalingAfspraakslots;
	}

	private RoosterItem getVolgendeAfspraak(RoosterItem afspraakslot, ColonAfspraakslotDto afspraakslotDto)
	{
		var afspraakslotClone = afspraakslot.transientClone();

		var volgendeStartTijd = getStartTijdVolgendeTijdslot(DateUtil.toLocalDateTime(afspraakslotClone.getStartTime()), afspraakslotDto);
		var volgendeEindTijd = getEindTijdVolgendeTijdslot(afspraakslotClone, volgendeStartTijd);

		afspraakslotClone.setStartTime(volgendeStartTijd);
		afspraakslotClone.setEndTime(volgendeEindTijd);
		afspraakslotClone.setRecurrence(null);
		return afspraakslotClone;
	}

	private Date getStartTijdVolgendeTijdslot(LocalDateTime huidigeTijdslotStartTijd, ColonAfspraakslotDto afspraakslotDto)
	{
		var herhalingDto = afspraakslotDto.getHerhaling();
		if (herhalingDto.getFrequentie() == ColonHerhalingsfrequentie.DAGELIJKS)
		{
			return getStartTijdVolgendeDagelijkseTijdslot(huidigeTijdslotStartTijd, herhalingDto.isAlleenWerkdagen());
		}
		var interval = herhalingDto.getFrequentie() == ColonHerhalingsfrequentie.TWEE_WEKELIJKS ? 2 : 1;
		return getStartTijdVolgendeWekelijkseTijdslot(huidigeTijdslotStartTijd, interval, herhalingDto.getDagen());
	}

	private Date getEindTijdVolgendeTijdslot(RoosterItem afspraakslot, Date volgendeStartTijd)
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
}
