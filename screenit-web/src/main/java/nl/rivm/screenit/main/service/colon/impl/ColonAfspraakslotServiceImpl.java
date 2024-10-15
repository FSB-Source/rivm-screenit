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

import javax.annotation.Nullable;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.exceptions.HeeftAfsprakenException;
import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.exceptions.TijdBlokOverlapException;
import nl.rivm.screenit.main.exception.BeperkingException;
import nl.rivm.screenit.main.exception.BulkAanmakenException;
import nl.rivm.screenit.main.exception.BulkVerwijderenException;
import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.main.service.colon.ColonAfspraakslotService;
import nl.rivm.screenit.main.service.colon.ColonFeestdagService;
import nl.rivm.screenit.main.service.colon.ColonRoosterBeperkingService;
import nl.rivm.screenit.main.service.colon.RoosterService;
import nl.rivm.screenit.mappers.colon.ColonAfspraakslotMapper;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.colon.ColonHerhalingsfrequentie;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.RoosterListViewFilter;
import nl.rivm.screenit.model.colon.dto.ColonAfspraakslotDto;
import nl.rivm.screenit.model.colon.dto.ColonHerhalingDto;
import nl.rivm.screenit.model.colon.dto.ColonRoosterBeperkingenDto;
import nl.rivm.screenit.model.colon.dto.ColonTijdslotDto;
import nl.rivm.screenit.model.colon.enums.ColonAfspraakStatus;
import nl.rivm.screenit.model.colon.enums.ColonAfspraakslotStatus;
import nl.rivm.screenit.model.colon.enums.ColonRoosterBeperking;
import nl.rivm.screenit.model.colon.enums.ColonTijdslotType;
import nl.rivm.screenit.model.colon.planning.ColonAfspraakslot;
import nl.rivm.screenit.model.colon.planning.ColonTijdslot;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.repository.colon.ColonAfspraakslotRepository;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.colon.ColonBaseAfspraakService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.collections.CollectionUtils;
import org.jetbrains.annotations.NotNull;
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

	private final LogService logService;

	private final ColonAfspraakslotRepository afspraakslotRepository;

	private final ColonAfspraakslotMapper afspraakslotMapper;

	private final ColonRoosterBeperkingService roosterBeperkingService;

	private final ColonFeestdagService feestdagService;

	private final ColonBaseAfspraakService afspraakService;

	private final OrganisatieParameterService organisatieParameterService;

	@Override
	@Transactional
	public void createAfspraakslot(ColonAfspraakslotDto afspraakslotDto, InstellingGebruiker instellingGebruiker)
		throws ValidatieException, OpslaanVerwijderenTijdBlokException, BeperkingException, BulkAanmakenException
	{
		var intakelocatie = roosterService.getIntakelocatieVanInstellingGebruiker(instellingGebruiker);
		var dbAfspraakslot = new ColonAfspraakslot();
		var afspraakslot = converteerAfspraakslot(afspraakslotDto, intakelocatie, dbAfspraakslot);
		var alleenValidatie = afspraakslotDto.isAlleenValidatie();

		if (afspraakslotDto.getHerhaling().getFrequentie() != ColonHerhalingsfrequentie.GEEN_HERHALING)
		{
			createBulkAfspraakslots(afspraakslot, afspraakslotDto, instellingGebruiker, intakelocatie, alleenValidatie);
		}
		else
		{
			createEnkelAfspraakslot(afspraakslot, afspraakslotDto, instellingGebruiker, intakelocatie, alleenValidatie);
		}
	}

	private void createEnkelAfspraakslot(ColonAfspraakslot afspraakslot, ColonAfspraakslotDto afspraakslotDto, InstellingGebruiker instellingGebruiker,
		ColonIntakelocatie intakelocatie,
		boolean alleenValidatie)
		throws ValidatieException, OpslaanVerwijderenTijdBlokException, BeperkingException
	{
		var aanTeMakenAfspraakslots = new ArrayList<ColonAfspraakslot>();
		aanTeMakenAfspraakslots.add(afspraakslot);

		valideerAfspraakslot(afspraakslot, intakelocatie, !alleenValidatie, false);
		if (!alleenValidatie)
		{
			logAction(afspraakslot, afspraakslotDto.getAantalBlokken(), instellingGebruiker, intakelocatie, null, LogGebeurtenis.AFSPRAAKSLOT_NIEUW,
				afspraakslotDto.getHerhaling());

			var transformedAfspraakslots = splitAfspraakslots(aanTeMakenAfspraakslots, afspraakslotDto.getAantalBlokken(), intakelocatie);
			afspraakslotRepository.saveAll(transformedAfspraakslots);
		}
	}

	private void createBulkAfspraakslots(ColonAfspraakslot afspraakslot, ColonAfspraakslotDto afspraakslotDto, InstellingGebruiker instellingGebruiker,
		ColonIntakelocatie intakelocatie,
		boolean alleenValidatie)
		throws BulkAanmakenException
	{
		var aanTeMakenAfspraakslots = new ArrayList<ColonAfspraakslot>();
		aanTeMakenAfspraakslots.add(afspraakslot);
		try
		{
			aanTeMakenAfspraakslots.addAll(roosterService.maakHerhalingTijdslotsAan(afspraakslot, afspraakslotDto.getHerhaling()));
			aanTeMakenAfspraakslots = (ArrayList<ColonAfspraakslot>) splitAfspraakslots(aanTeMakenAfspraakslots, afspraakslotDto.getAantalBlokken(), intakelocatie);
			valideerBulkAfspraakslots(aanTeMakenAfspraakslots, intakelocatie);

			if (!alleenValidatie)
			{
				logAction(afspraakslot, afspraakslotDto.getAantalBlokken(), instellingGebruiker, intakelocatie, null, LogGebeurtenis.AFSPRAAKSLOT_NIEUW,
					afspraakslotDto.getHerhaling());

				afspraakslotRepository.saveAll(aanTeMakenAfspraakslots);
			}
		}
		catch (BulkAanmakenException ex)
		{

			if (alleenValidatie)
			{
				throw ex;
			}

			else if (!aanTeMakenAfspraakslots.isEmpty())
			{
				var melding = genereerLogMessage(aanTeMakenAfspraakslots.get(0), null, LogGebeurtenis.AFSPRAAKSLOT_NIEUW, afspraakslotDto.getAantalBlokken(),
					afspraakslotDto.getHerhaling());
				melding += ". " + ex.getSamenvatting();
				logService.logGebeurtenis(LogGebeurtenis.AFSPRAAKSLOT_NIEUW, instellingGebruiker, melding, Bevolkingsonderzoek.COLON);

				afspraakslotRepository.saveAll(aanTeMakenAfspraakslots);
			}
		}
	}

	@Override
	@Transactional
	public void updateAfspraakslot(Long id, ColonAfspraakslotDto afspraakslotDto, InstellingGebruiker instellingGebruiker)
		throws ValidatieException, OpslaanVerwijderenTijdBlokException, IllegalStateException, BeperkingException
	{
		var intakelocatie = roosterService.getIntakelocatieVanInstellingGebruiker(instellingGebruiker);
		var dbAfspraakslot = roosterService.getAfspraakslot(id).orElseThrow(() -> new IllegalStateException("Afspraakslot kan niet worden gevonden"));

		var originalAfspraakslot = dbAfspraakslot.transientClone();
		var validateAfspraakslot = dbAfspraakslot.transientClone();
		var alleenValidatie = afspraakslotDto.isAlleenValidatie();

		validateAfspraakslot.setId(id);
		hibernateService.getHibernateSession().detach(validateAfspraakslot); 
		converteerAfspraakslot(afspraakslotDto, intakelocatie, validateAfspraakslot);
		valideerAfspraakslot(validateAfspraakslot, intakelocatie, !alleenValidatie, true);

		if (!alleenValidatie)
		{
			converteerAfspraakslot(afspraakslotDto, intakelocatie, dbAfspraakslot);
			logAction(dbAfspraakslot, afspraakslotDto.getAantalBlokken(), instellingGebruiker, intakelocatie, originalAfspraakslot, LogGebeurtenis.AFSPRAAKSLOT_WIJZIG, null);
			afspraakslotRepository.save(dbAfspraakslot);
		}
	}

	@Override
	@Transactional
	public void deleteAfspraakslot(Long id, InstellingGebruiker instellingGebruiker)
		throws OpslaanVerwijderenTijdBlokException, ValidatieException
	{
		var intakelocatie = roosterService.getIntakelocatieVanInstellingGebruiker(instellingGebruiker);
		var dbAfspraakslot = roosterService.getAfspraakslot(id).orElseThrow(() -> new ValidatieException("error.afspraakslot.niet.gevonden"));
		var afspraakslotStatus = roosterService.getAfspraakslotStatus(dbAfspraakslot);
		magAfsrpaakslotOpslaanVerwijderen(dbAfspraakslot, false);

		if (afspraakslotStatus == ColonAfspraakslotStatus.GEBRUIKT_VOOR_CAPACITEIT)
		{
			throw new ValidatieException("error.afspraakslot.gebruikt.voor.capaciteit");
		}

		afspraakslotRepository.delete(dbAfspraakslot);
		logAction(dbAfspraakslot, 1, instellingGebruiker, intakelocatie, dbAfspraakslot, LogGebeurtenis.AFSPRAAKSLOT_VERWIJDEREN, null);
	}

	@Override
	@Transactional
	public void bulkDeleteAfspraakslots(List<Long> ids, InstellingGebruiker instellingGebruiker, boolean alleenValidatie)
		throws BulkVerwijderenException
	{
		var exception = new BulkVerwijderenException(ColonTijdslotType.AFSPRAAKSLOT);
		var teVerwijderenSlots = new ArrayList<ColonAfspraakslot>();
		ids.forEach(id ->
		{
			try
			{
				var dbAfspraakslot = roosterService.getAfspraakslot(id).orElse(null);
				if (dbAfspraakslot == null)
				{
					exception.aantalNietGevondenOphogen();
					return;
				}
				var afspraakslotStatus = roosterService.getAfspraakslotStatus(dbAfspraakslot);
				magAfsrpaakslotOpslaanVerwijderen(dbAfspraakslot, true);

				if (afspraakslotStatus == ColonAfspraakslotStatus.GEBRUIKT_VOOR_CAPACITEIT)
				{
					exception.aantalGebruiktVoorCapaciteitOphogen();
					return;
				}
				exception.aantalVerwijderenOphogen();
				teVerwijderenSlots.add(dbAfspraakslot);
			}

			catch (OpslaanVerwijderenTijdBlokException ex)
			{
				exception.aantalMetAfspraakOphogen();
			}
		});

		if (alleenValidatie)
		{
			throw exception;
		}
		else
		{
			afspraakslotRepository.deleteAll(teVerwijderenSlots);
			logService.logGebeurtenis(LogGebeurtenis.AFSPRAAKSLOT_VERWIJDEREN, instellingGebruiker, exception.getMessage(), Bevolkingsonderzoek.COLON);
		}
	}

	@Override
	public List<ColonAfspraakslotDto> getAfspraakslots(LocalDate startDate, LocalDate endDate, ColonIntakelocatie intakeLocatie)
	{
		var filter = new RoosterListViewFilter();
		filter.setStartDatum(DateUtil.toUtilDate(startDate));
		filter.setEindDatum(DateUtil.toUtilDate(endDate));

		var gevondenAfspraakslots = roosterService.getAlleAfspraakslotsInPeriode("vanaf", true, filter, intakeLocatie);
		var afspraakslots = new ArrayList<ColonAfspraakslotDto>();
		for (var gevondenAfspraakslot : gevondenAfspraakslots)
		{
			var afspraakslot = afspraakslotRepository.findById(gevondenAfspraakslot.getAfspraakslotId()).orElse(null);
			var afspraakslotStatus = roosterService.getAfspraakslotStatus(afspraakslot);
			gevondenAfspraakslot.setStatus(afspraakslotStatus);
			afspraakslots.add(afspraakslotMapper.roosterListItemViewWrapperToColonAfspraakslotDto(gevondenAfspraakslot));
		}
		return afspraakslots;
	}

	@Override
	public List<ColonTijdslotDto> searchAfspraakslots(RoosterListViewFilter filter, long intakelocatieId)
	{
		return roosterService.searchTijdslots(filter, intakelocatieId, ColonTijdslotType.AFSPRAAKSLOT);
	}

	@Override
	public void checkEindTijdOpZelfdeDag(LocalDateTime startDateTime, LocalDateTime endDateTime, ColonIntakelocatie intakelocatie) throws ValidatieException
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
	public void checkCapaciteitBerekening(ColonAfspraakslot afspraakslot, ColonIntakelocatie intakelocatie) throws ValidatieException
	{
		var afspraakslotStatus = roosterService.getAfspraakslotStatus(afspraakslot);
		if (afspraakslotStatus != ColonAfspraakslotStatus.GEBRUIKT_VOOR_CAPACITEIT || afspraakslot.getId() == null)
		{
			return;
		}

		var vanaf = afspraakslot.getVanaf();
		var origAfspraakslot = EntityAuditUtil.getPreviousVersionOfEntity(afspraakslot, hibernateService.getHibernateSession());
		if (origAfspraakslot == null)
		{
			return;
		}

		var origStartTime = origAfspraakslot.getVanaf().toLocalDate();
		var duurAfspraakInMinuten = getDuurAfspraakInMinuten(intakelocatie);
		var newEindDatumGebruiktVoorCapaciteit = origStartTime.plusWeeks(1).with(TemporalAdjusters.previousOrSame(DayOfWeek.MONDAY))
			.atStartOfDay();
		var origStartRange = Range.openClosed(newEindDatumGebruiktVoorCapaciteit.minusWeeks(1), newEindDatumGebruiktVoorCapaciteit);
		var startRange = Range.openClosed(vanaf, vanaf.plusMinutes(duurAfspraakInMinuten));

		if (!DateUtil.overlapsLocalDateTime(origStartRange, startRange))
		{
			throw new ValidatieException("error.afspraakslot.gebruikt.voor.capaciteit");
		}
	}

	private void valideerAfspraakslot(ColonAfspraakslot afspraakslot, ColonIntakelocatie intakelocatie, boolean negeerZachteBeperking, boolean wijzigen)
		throws ValidatieException, OpslaanVerwijderenTijdBlokException, BeperkingException
	{
		valideerBeperkingen(afspraakslot, ColonRoosterBeperking.HARD);
		roosterService.valideerTijdslot(afspraakslot);
		magAfsrpaakslotOpslaanVerwijderen(afspraakslot, wijzigen);

		checkCapaciteitBerekening(afspraakslot, intakelocatie);
		checkEindTijdOpZelfdeDag(afspraakslot.getVanaf(), afspraakslot.getTot(), intakelocatie);

		if (!negeerZachteBeperking)
		{
			valideerBeperkingen(afspraakslot, ColonRoosterBeperking.ZACHT);
		}
	}

	private void valideerBulkAfspraakslots(List<ColonAfspraakslot> aanTeMakenAfspraakslots, ColonIntakelocatie intakelocatie)
		throws BulkAanmakenException
	{
		var bulkAanmakenException = new BulkAanmakenException();
		var invalideAfspraakslots = new ArrayList<ColonAfspraakslot>();
		for (var afspraakslot : aanTeMakenAfspraakslots)
		{
			try
			{
				valideerBeperkingen(afspraakslot, ColonRoosterBeperking.HARD);
				roosterService.valideerTijdslot(afspraakslot);
				magAfsrpaakslotOpslaanVerwijderen(afspraakslot, true);
				checkCapaciteitBerekening(afspraakslot, intakelocatie);
				checkEindTijdOpZelfdeDag(afspraakslot.getVanaf(), afspraakslot.getTot(), intakelocatie);
				valideerBeperkingen(afspraakslot, ColonRoosterBeperking.ZACHT);
			}
			catch (ValidatieException | OpslaanVerwijderenTijdBlokException | BeperkingException ex)
			{
				bulkAanmakenException.addException(afspraakslot, ex);

				if (!(ex instanceof BeperkingException && ((BeperkingException) ex).getBeperkingType() == ColonRoosterBeperking.ZACHT))
				{
					invalideAfspraakslots.add(afspraakslot);
				}
			}
		}

		aanTeMakenAfspraakslots.removeAll(invalideAfspraakslots);

		if (!bulkAanmakenException.isEmpty())
		{
			throw bulkAanmakenException;
		}
	}

	public void magAfsrpaakslotOpslaanVerwijderen(ColonAfspraakslot afspraakslot, boolean wijzigen)
		throws OpslaanVerwijderenTijdBlokException
	{
		if (afspraakslot.getId() != null)
		{

			var afsprakenObjects = intakeafspraakVanAfspraakslot(afspraakslot);
			if (!afsprakenObjects.isEmpty())
			{

				if (wijzigen)
				{
					throw new HeeftAfsprakenException("error.afspraakslot.wijzig.heeft.afspraken", afsprakenObjects);
				}
				else
				{
					throw new HeeftAfsprakenException("error.afspraakslot.verwijder.heeft.afspraken", afsprakenObjects);
				}
			}
		}

		var bereikNieuwAfspraakslot = Range.open(DateUtil.startMinuut(afspraakslot.getVanaf()), DateUtil.startMinuut(afspraakslot.getTot()));

		List<ColonAfspraakslot> overlapteAfspraakslots = null;
		if (wijzigen || afspraakslot.getId() == null) 
		{
			overlapteAfspraakslots = roosterService.getAfspraakslotsInRangeEnKamer(bereikNieuwAfspraakslot, afspraakslot);
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
			var echteOverlapteAfspraakslots = new ArrayList<Range<LocalDateTime>>();

			for (var overlapteAfspraakslot : overlapteAfspraakslots)
			{
				var startDateTimeBestaand = DateUtil.startMinuut(overlapteAfspraakslot.getVanaf());
				var endDateTimeBestaand = DateUtil.startMinuut(overlapteAfspraakslot.getTot());
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

	private @NotNull List<Object> intakeafspraakVanAfspraakslot(ColonAfspraakslot afspraakslot)
	{

		var afspraak = afspraakslotRepository.findById(afspraakslot.getId()).orElseThrow().getAfspraak();
		if (afspraak != null && List.of(ColonAfspraakStatus.GEPLAND, ColonAfspraakStatus.UITGEVOERD).contains(afspraak.getStatus()))
		{
			return List.of(afspraak);
		}
		return List.of();
	}

	@Override
	public void valideerBeperkingen(ColonTijdslot tijdslot, ColonRoosterBeperking beperkingType) throws BeperkingException
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

	private void valideerNachtBeperkingBegin(ColonTijdslot tijdslot, ColonRoosterBeperking beperkingType, BeperkingException exceptie, ColonRoosterBeperkingenDto beperkingen)
	{
		if (beperkingen.getNachtBeperkingType() == beperkingType && beperkingen.getNachtBeperkingBegin().isBefore(tijdslot.getTot().toLocalTime()))
		{
			exceptie.addException(new ValidatieException(beperkingType == ColonRoosterBeperking.HARD ? "error.harde.nachtbeperking.begin" : "error.zachte.nachtbeperking.begin",
				beperkingen.getNachtBeperkingBegin().toString()));
		}
	}

	private void valideerNachtBeperkingEind(ColonTijdslot tijdslot, ColonRoosterBeperking beperkingType, BeperkingException exceptie, ColonRoosterBeperkingenDto beperkingen)
	{
		if (beperkingen.getNachtBeperkingType() == beperkingType && beperkingen.getNachtBeperkingEind().isAfter(tijdslot.getVanaf().toLocalTime()))
		{
			exceptie.addException(new ValidatieException(beperkingType == ColonRoosterBeperking.HARD ? "error.harde.nachtbeperking.eind" : "error.zachte.nachtbeperking.eind",
				beperkingen.getNachtBeperkingEind().toString()));
		}
	}

	private void valideerZaterdagBeperking(ColonTijdslot tijdslot, ColonRoosterBeperking beperkingType, BeperkingException exceptie, ColonRoosterBeperkingenDto beperkingen)
	{
		if (beperkingen.getZaterdagBeperkingType() == beperkingType && tijdslot.getVanaf().getDayOfWeek() == DayOfWeek.SATURDAY)
		{
			exceptie.addException(
				new ValidatieException(beperkingType == ColonRoosterBeperking.HARD ? "error.harde.weekendbeperking.zaterdag" : "error.zachte.weekendbeperking.zaterdag"));
		}
	}

	private void valideerZondagBeperking(ColonTijdslot tijdslot, ColonRoosterBeperking beperkingType, BeperkingException exceptie, ColonRoosterBeperkingenDto beperkingen)
	{
		if (beperkingen.getZondagBeperkingType() == beperkingType && tijdslot.getVanaf().getDayOfWeek() == DayOfWeek.SUNDAY)
		{
			exceptie.addException(
				new ValidatieException(beperkingType == ColonRoosterBeperking.HARD ? "error.harde.weekendbeperking.zondag" : "error.zachte.weekendbeperking.zondag"));
		}
	}

	private void valideerFeestdagBeperking(ColonTijdslot tijdslot, ColonRoosterBeperking beperkingType, BeperkingException exceptie)
	{
		var feestdagen = feestdagService.getFeestdagen();
		var feestdagOpDagTijdslot = feestdagen.stream().filter(feestdag -> DateUtil.isZelfdeDag(tijdslot.getVanaf().toLocalDate(), DateUtil.toUtilDate(feestdag.getDatum())))
			.findFirst()
			.orElse(null);
		if (feestdagOpDagTijdslot != null && feestdagOpDagTijdslot.getBeperking() == beperkingType)
		{
			exceptie.addException(
				new ValidatieException(beperkingType == ColonRoosterBeperking.HARD ? "error.harde.feestdagbeperking" : "error.zachte.feestdagbeperking",
					feestdagOpDagTijdslot.getNaam()));
		}
	}

	private ColonAfspraakslot converteerAfspraakslot(ColonAfspraakslotDto afspraakslotDto, ColonIntakelocatie intakelocatie, ColonAfspraakslot afspraakslot)
	{
		afspraakslot.setVanaf(afspraakslotDto.getVanaf());
		afspraakslot.setTot(afspraakslotDto.getTot());

		var kamer = intakelocatie.getKamers().stream().filter(k -> k.getId().equals(afspraakslotDto.getKamerId())).findFirst()
			.orElse(null);
		afspraakslot.setKamer(kamer);

		return afspraakslot;
	}

	private Integer getDuurAfspraakInMinuten(ColonIntakelocatie intakelocatie)
	{
		return organisatieParameterService.getOrganisatieParameter(intakelocatie, OrganisatieParameterKey.COLON_DUUR_AFSPRAAK_IN_MINUTEN, 15);
	}

	private List<ColonAfspraakslot> splitAfspraakslots(List<ColonAfspraakslot> aanTeMakenAfspraakslots, Integer aantalBlokken, ColonIntakelocatie intakelocatie)
	{
		var afspraakslots = new ArrayList<ColonAfspraakslot>();
		var duurAfspraakslotsInMinuten = getDuurAfspraakInMinuten(intakelocatie);
		for (var afspraakslot : aanTeMakenAfspraakslots)
		{
			var startDatumTijd = afspraakslot.getVanaf();

			for (int i = 0; i < aantalBlokken; i++)
			{
				var splittedAfspraakslot = afspraakslot.transientClone();
				splittedAfspraakslot.setVanaf(startDatumTijd);
				var eindDatumTijd = startDatumTijd.plusMinutes(duurAfspraakslotsInMinuten);
				splittedAfspraakslot.setTot(eindDatumTijd);
				startDatumTijd = eindDatumTijd;
				afspraakslots.add(splittedAfspraakslot);
			}
		}
		return afspraakslots;
	}

	public void logAction(ColonAfspraakslot unsavedObject, int aantalBlokken, InstellingGebruiker instellingGebruiker, ColonIntakelocatie intakelocatie,
		@Nullable ColonAfspraakslot origineleAfspraakslot, LogGebeurtenis gebeurtenis, ColonHerhalingDto herhalingDto)
	{
		var melding = genereerLogMessage(unsavedObject, origineleAfspraakslot, gebeurtenis, aantalBlokken, herhalingDto);
		logService.logGebeurtenis(gebeurtenis, instellingGebruiker, melding, Bevolkingsonderzoek.COLON);
	}

	private String genereerLogMessage(ColonAfspraakslot unsavedAfspraakslot, @Nullable ColonAfspraakslot originalAfspraakslot,
		LogGebeurtenis gebeurtenis, int aantalBlokken, ColonHerhalingDto herhalingDto)
	{
		var dateTimeFormat = DateUtil.LOCAL_DATE_TIME_FORMAT;
		var melding = dateTimeFormat.format(unsavedAfspraakslot.getVanaf()) + ", " + unsavedAfspraakslot.getKamer().getNaam() + ", " + unsavedAfspraakslot.getKamer()
			.getIntakelocatie()
			.getNaam();

		if (herhalingDto != null && herhalingDto.getFrequentie() != ColonHerhalingsfrequentie.GEEN_HERHALING)
		{
			melding += ", " + herhalingDto.getFrequentie().getNaam() + " t/m " + herhalingDto.getEindDatum().format(DateUtil.LOCAL_DATE_FORMAT);
		}

		if (gebeurtenis == LogGebeurtenis.AFSPRAAKSLOT_NIEUW)
		{
			melding = "#" + aantalBlokken + " slots, " + melding;
		}
		else if (gebeurtenis == LogGebeurtenis.AFSPRAAKSLOT_WIJZIG && originalAfspraakslot != null)
		{
			var origStartTime = originalAfspraakslot.getVanaf();
			melding = dateTimeFormat.format(origStartTime) + " -> " + melding;

		}

		return melding;
	}
}
