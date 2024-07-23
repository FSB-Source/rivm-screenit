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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import javax.annotation.Nullable;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.exceptions.HeeftAfsprakenException;
import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.exceptions.TijdBlokOverlapException;
import nl.rivm.screenit.main.exception.BulkAanmakenException;
import nl.rivm.screenit.main.exception.BulkVerwijderenException;
import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.main.service.colon.ColonBlokkadeService;
import nl.rivm.screenit.main.service.colon.RoosterService;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.colon.ColonHerhalingsfrequentie;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.RoosterListViewFilter;
import nl.rivm.screenit.model.colon.dto.ColonBlokkadeDto;
import nl.rivm.screenit.model.colon.dto.ColonHerhalingDto;
import nl.rivm.screenit.model.colon.dto.ColonTijdslotDto;
import nl.rivm.screenit.model.colon.enums.ColonTijdSlotType;
import nl.rivm.screenit.model.colon.planning.ColonBlokkade;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.repository.colon.ColonBlokkadeRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.colon.ColonBaseAfspraakService;
import nl.rivm.screenit.specification.colon.ColonBlokkadeSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment_;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.Range;

@AllArgsConstructor
@Service
@Slf4j
public class ColonBlokkadeServiceImpl implements ColonBlokkadeService
{
	private final RoosterService roosterService;

	private final LogService logService;

	private final ColonBlokkadeRepository blokkadeRepository;

	private final ICurrentDateSupplier currentDateSupplier;

	private final ColonBaseAfspraakService afspraakService;

	@Override
	@Transactional
	public void createBlokkade(ColonBlokkadeDto blokkadeDto, InstellingGebruiker instellingGebruiker)
		throws ValidatieException, OpslaanVerwijderenTijdBlokException, BulkAanmakenException
	{
		var intakelocatie = roosterService.getIntakelocatieVanInstellingGebruiker(instellingGebruiker);
		var dbBlokkade = new ColonBlokkade();
		var blokkade = converteerBlokkade(blokkadeDto, intakelocatie, dbBlokkade);
		var alleenValidatie = blokkadeDto.isAlleenValidatie();
		if (blokkadeDto.getHerhaling().getFrequentie() != ColonHerhalingsfrequentie.GEEN_HERHALING)
		{
			createBulkBlokkades(blokkade, blokkadeDto, instellingGebruiker, intakelocatie, alleenValidatie);
		}
		else
		{
			createEnkeleBlokkade(blokkade, blokkadeDto, instellingGebruiker, intakelocatie, alleenValidatie);
		}
	}

	private void createEnkeleBlokkade(ColonBlokkade blokkade, ColonBlokkadeDto blokkadeDto, InstellingGebruiker instellingGebruiker, ColoscopieCentrum intakelocatie,
		boolean alleenValidatie) throws ValidatieException, OpslaanVerwijderenTijdBlokException
	{
		valideerBlokkade(blokkade, getCurrentViewRange(blokkadeDto), getBlokkadeKamers(blokkade, blokkadeDto.getAlleKamers(), intakelocatie), false, false);
		if (!alleenValidatie)
		{
			var blokkadesPerKamer = splitBlokkade(blokkade, blokkadeDto.getAlleKamers(), intakelocatie);
			logAction(blokkade, instellingGebruiker, intakelocatie, null, LogGebeurtenis.COLON_BLOKKADES_NIEUW, null, null);
			blokkadeRepository.saveAll(blokkadesPerKamer);
		}
	}

	private static List<Kamer> getBlokkadeKamers(ColonBlokkade blokkade, boolean alleKamers, ColoscopieCentrum intakelocatie)
	{
		return alleKamers ? getActieveKamers(intakelocatie) : Arrays.asList(blokkade.getLocation());
	}

	private void createBulkBlokkades(ColonBlokkade blokkade, ColonBlokkadeDto blokkadeDto, InstellingGebruiker instellingGebruiker, ColoscopieCentrum intakelocatie,
		boolean alleenValidatie)
		throws BulkAanmakenException
	{
		var aanTeMakenBlokkades = new ArrayList<ColonBlokkade>();
		final var blokkadesPerKamer = new ArrayList<ColonBlokkade>();
		aanTeMakenBlokkades.add(blokkade);
		try
		{
			aanTeMakenBlokkades.addAll(roosterService.maakHerhalingTijdslotsAan(blokkade, blokkadeDto.getHerhaling()));

			aanTeMakenBlokkades.forEach(b -> blokkadesPerKamer.addAll(splitBlokkade(b, blokkadeDto.getAlleKamers(), intakelocatie)));

			valideerBulkBlokkades(blokkadesPerKamer, alleenValidatie);

			if (!alleenValidatie)
			{
				logAction(blokkade, instellingGebruiker, intakelocatie, null, LogGebeurtenis.COLON_BLOKKADES_NIEUW, blokkadeDto.getHerhaling(), null);

				blokkadeRepository.saveAll(blokkadesPerKamer);
			}
		}
		catch (BulkAanmakenException ex)
		{

			if (alleenValidatie)
			{
				throw ex;
			}

			else if (!blokkadesPerKamer.isEmpty())
			{
				logAction(blokkade, instellingGebruiker, intakelocatie, null, LogGebeurtenis.COLON_BLOKKADES_NIEUW, blokkadeDto.getHerhaling(), ex);

				blokkadeRepository.saveAll(blokkadesPerKamer);
			}
		}
	}

	private void valideerBulkBlokkades(List<ColonBlokkade> aanTeMakenBlokkades, boolean alleenValidatie)
		throws BulkAanmakenException
	{
		var bulkAanmakenException = new BulkAanmakenException();
		var invalideBlokkades = new ArrayList<ColonBlokkade>();
		for (var blokkade : aanTeMakenBlokkades)
		{
			try
			{
				var currentViewRange = Range.closed(blokkade.getStartTime(), blokkade.getEndTime());
				valideerBlokkade(blokkade, currentViewRange, List.of(blokkade.getLocation()), alleenValidatie, true);
			}
			catch (ValidatieException | OpslaanVerwijderenTijdBlokException ex)
			{
				bulkAanmakenException.addException(blokkade, ex);
				invalideBlokkades.add(blokkade);
			}
		}

		aanTeMakenBlokkades.removeAll(invalideBlokkades);

		if (!bulkAanmakenException.isEmpty())
		{
			throw bulkAanmakenException;
		}
	}

	private List<ColonBlokkade> splitBlokkade(ColonBlokkade unsavedBlokkade, boolean alleKamers, ColoscopieCentrum intakelocatie)
	{
		var list = new ArrayList<ColonBlokkade>();
		if (unsavedBlokkade.getLocation() != null)
		{
			list.add(unsavedBlokkade);
		}
		if (alleKamers)
		{
			for (var kamer : getActieveKamers(intakelocatie))
			{
				if (!Objects.equals(kamer, unsavedBlokkade.getLocation()))
				{
					var blokkade = unsavedBlokkade.transientClone();
					blokkade.setLocation(kamer);
					list.add(blokkade);
				}
			}
		}
		return list;
	}

	private Range<Date> getCurrentViewRange(ColonBlokkadeDto blokkade)
	{
		var startDatumTijd = DateUtil.toUtilDate(blokkade.getStartTime());
		var eindDatumTijd = DateUtil.toUtilDate(blokkade.getEndTime());
		if (blokkade.getHerhaling() != null && !blokkade.getHerhaling().getFrequentie().equals(ColonHerhalingsfrequentie.GEEN_HERHALING))
		{
			eindDatumTijd = DateUtil.toUtilDate(blokkade.getHerhaling().getEindDatum());
		}
		return Range.closed(startDatumTijd, eindDatumTijd);
	}

	private void valideerBlokkade(ColonBlokkade blokkade, Range<Date> currentViewRange, List<Kamer> kamers, boolean wijzigen, boolean bulk)
		throws ValidatieException, OpslaanVerwijderenTijdBlokException
	{
		roosterService.valideerTijdslot(blokkade);
		heeftOverlappendeBlokkades(blokkade, kamers, true);
		heeftAfspraken(blokkade, currentViewRange, kamers, wijzigen, bulk);
	}

	public void heeftAfspraken(ColonBlokkade blokkade, Range<Date> currentViewRange, List<Kamer> kamers, boolean wijzigen, boolean bulk) throws HeeftAfsprakenException
	{
		var afspraken = new ArrayList<>();
		kamers.forEach(kamer -> afspraken.addAll(afspraakService.getAfsprakenKamersInRange(kamer, currentViewRange)));

		List<Object> afspraakObjects = new ArrayList<>(afspraken);

		if (!afspraken.isEmpty())
		{
			if (bulk)
			{
				throw new HeeftAfsprakenException("error.blokkade.bulk.heeft.afspraken", List.of());
			}
			else if (blokkade.getId() == null)
			{
				throw new HeeftAfsprakenException("error.blokkade.nieuw.heeft.afspraken", afspraakObjects);
			}
			else if (wijzigen)
			{
				throw new HeeftAfsprakenException("error.blokkade.wijzig.heeft.afspraken", afspraakObjects);
			}
		}
	}

	private void heeftOverlappendeBlokkades(ColonBlokkade blokkade, List<Kamer> kamers, boolean wijzigen) throws OpslaanVerwijderenTijdBlokException
	{

		var nieuweBlokkade = Range.closed(DateUtil.startMinuut(blokkade.getStartTime()), DateUtil.startMinuut(blokkade.getEndTime()));

		List<ColonBlokkade> overlapteBlokkades = null;
		if (wijzigen || blokkade.getId() == null) 
		{
			overlapteBlokkades = getBlokkadeTijden(nieuweBlokkade, kamers);
		}

		if (blokkade.getId() == null) 
		{
			if (CollectionUtils.isNotEmpty(overlapteBlokkades))
			{

				throw new TijdBlokOverlapException("error.blokkade.heeft.overlap", overlapteBlokkades);
			}
		}
		else if (wijzigen)
		{
			var echteOverlapteBlokkades = new ArrayList<Range<Date>>();

			for (var overlapteBlokkade : overlapteBlokkades)
			{
				var startDateTimeBestaand = DateUtil.startMinuut(overlapteBlokkade.getStartTime());
				var endDateTimeBestaand = DateUtil.startMinuut(overlapteBlokkade.getEndTime());
				var overlapteBlokkadeRange = Range.closed(startDateTimeBestaand, endDateTimeBestaand);

				if (!blokkade.getId().equals(overlapteBlokkade.getId()))
				{

					echteOverlapteBlokkades.add(overlapteBlokkadeRange);
				}
			}
			if (!echteOverlapteBlokkades.isEmpty())
			{

				throw new TijdBlokOverlapException("error.blokkade.heeft.overlap", echteOverlapteBlokkades);
			}
		}
	}

	private void magVerwijderen(ColonBlokkade blokkade) throws ValidatieException
	{
		if (blokkade == null)
		{
			throw new ValidatieException("error.blokkade.niet.gevonden");
		}

		if (DateUtil.compareBefore(blokkade.getEndTime(), currentDateSupplier.getDate()))
		{
			throw new ValidatieException("error.blokkade.in.verleden.verwijderen");
		}
	}

	@Override
	@Transactional
	public void updateBlokkade(ColonBlokkadeDto blokkadeDto, InstellingGebruiker loggedInInstellingGebruiker)
		throws OpslaanVerwijderenTijdBlokException, ValidatieException
	{
		var blokkadeId = blokkadeDto.getId();
		var intakelocatie = roosterService.getIntakelocatieVanInstellingGebruiker(loggedInInstellingGebruiker);
		var blokkade = getBlokkade(blokkadeId).orElseThrow(() -> new IllegalStateException("Blokkade kan niet worden gevonden"));
		var alleenValidatie = blokkadeDto.isAlleenValidatie();

		var originalBlokkade = blokkade.transientClone();
		var validateBlokkade = blokkade.transientClone();
		validateBlokkade.setId(blokkadeId);
		converteerBlokkade(blokkadeDto, intakelocatie, validateBlokkade);
		valideerBlokkade(validateBlokkade, getCurrentViewRange(blokkadeDto), List.of(blokkade.getLocation()), true, false);

		if (!alleenValidatie)
		{
			converteerBlokkade(blokkadeDto, intakelocatie, blokkade);
			logAction(blokkade, loggedInInstellingGebruiker, intakelocatie, originalBlokkade, LogGebeurtenis.COLON_BLOKKADES_WIJZIG, null, null);
			blokkadeRepository.save(blokkade);
		}
	}

	@Override
	@Transactional
	public void deleteBlokkade(Long blokkadeId, InstellingGebruiker instellingGebruiker) throws ValidatieException
	{
		var blokkade = getBlokkade(blokkadeId).orElse(null);
		var intakelocatie = roosterService.getIntakelocatieVanInstellingGebruiker(instellingGebruiker);

		magVerwijderen(blokkade);

		blokkadeRepository.delete(blokkade);
		logAction(blokkade, instellingGebruiker, intakelocatie, null, LogGebeurtenis.COLON_BLOKKADES_VERWIJDEREN, null, null);
	}

	@Override
	public Optional<ColonBlokkade> getBlokkade(Long blokkadeId)
	{
		return blokkadeRepository.findById(blokkadeId);
	}

	private static List<Kamer> getActieveKamers(ColoscopieCentrum intakelocatie)
	{
		return intakelocatie.getKamers().stream().filter(Kamer::getActief).collect(Collectors.toList());
	}

	private List<ColonBlokkade> getBlokkadeTijden(Range<Date> range, List<Kamer> kamers)
	{
		var specification = ColonBlokkadeSpecification.valtBinnenDatumRange(range);
		if (!kamers.isEmpty())
		{

			specification = specification.and(ColonBlokkadeSpecification.heeftKamerUitLijst(kamers));
		}
		return blokkadeRepository.findAll(specification, Sort.by(Sort.Direction.ASC, AbstractAppointment_.START_TIME));
	}

	private ColonBlokkade converteerBlokkade(ColonBlokkadeDto blokkadeDto, ColoscopieCentrum intakelocatie, ColonBlokkade dbBlokkade)
	{
		dbBlokkade.setStartTime(DateUtil.toUtilDate(blokkadeDto.getStartTime()));
		dbBlokkade.setEndTime(DateUtil.toUtilDate(blokkadeDto.getEndTime()));
		dbBlokkade.setDescription(blokkadeDto.getDescription());
		if (!Boolean.TRUE.equals(blokkadeDto.getAlleKamers()))
		{
			var kamer = intakelocatie.getKamers().stream().filter(k -> k.getId().equals(blokkadeDto.getKamerId())).findFirst()
				.orElse(null);
			dbBlokkade.setLocation(kamer);
		}

		return dbBlokkade;
	}

	@Override
	@Transactional
	public void logAction(ColonBlokkade unsavedObject, InstellingGebruiker instellingGebruiker, ColoscopieCentrum intakelocatie, @Nullable ColonBlokkade origineleBlokkade,
		LogGebeurtenis logGebeurtenis, ColonHerhalingDto herhalingDto, Exception ex)
	{
		var selectedKamer = unsavedObject.getLocation();
		if (selectedKamer != null)
		{
			logActionKamer(unsavedObject, instellingGebruiker, selectedKamer, origineleBlokkade, logGebeurtenis, herhalingDto, ex);
		}
		else
		{
			for (Kamer kamer : getActieveKamers(intakelocatie))
			{
				logActionKamer(unsavedObject, instellingGebruiker, kamer, origineleBlokkade, logGebeurtenis, herhalingDto, ex);
			}
		}
	}

	private void logActionKamer(ColonBlokkade unsavedObject, InstellingGebruiker instellingGebruiker, Kamer kamer, @Nullable ColonBlokkade origineleBlokkade,
		LogGebeurtenis logGebeurtenis, ColonHerhalingDto herhalingDto, Exception ex)
	{
		var melding = genereerLogMessage(unsavedObject, kamer, origineleBlokkade, logGebeurtenis, herhalingDto);
		if (ex instanceof BulkAanmakenException)
		{
			melding += ". " + ((BulkAanmakenException) ex).getSamenvatting();
		}
		else if (ex != null)
		{
			melding += ". " + ex.getMessage();
		}
		logService.logGebeurtenis(logGebeurtenis, instellingGebruiker, melding, Bevolkingsonderzoek.COLON);
	}

	private String genereerLogMessage(ColonBlokkade unsavedObject, Kamer kamer, @Nullable ColonBlokkade origineleBlokkade,
		LogGebeurtenis logGebeurtenis, ColonHerhalingDto herhalingDto)
	{
		var melding = getPeriodeTekst(unsavedObject) + ", " + kamer.getName() + ", " + kamer.getColoscopieCentrum().getNaam();
		if (herhalingDto != null && herhalingDto.getFrequentie() != ColonHerhalingsfrequentie.GEEN_HERHALING)
		{
			melding += ", " + herhalingDto.getFrequentie().getNaam() + " t/m " + herhalingDto.getEindDatum().format(DateUtil.LOCAL_DATE_FORMAT);
		}

		if (logGebeurtenis == LogGebeurtenis.COLON_BLOKKADES_WIJZIG && origineleBlokkade != null)
		{
			melding = DateUtil.formatShortDateTime(origineleBlokkade.getStartTime()) + " -> " + melding;
		}
		return melding;
	}

	@Override
	public String getPeriodeTekst(ColonBlokkade unsavedBlokkade)
	{
		var periodeTekst = "";
		if (unsavedBlokkade != null && unsavedBlokkade.getStartTime() != null && unsavedBlokkade.getEndTime() != null)
		{
			periodeTekst = DateUtil.formatShortDate(unsavedBlokkade.getStartTime()) + " ";
			if (!DateUtil.isZelfdeDag(unsavedBlokkade.getEndTime(), unsavedBlokkade.getStartTime()))
			{
				periodeTekst += "hele dag";
			}
			else
			{
				periodeTekst += DateUtil.formatTime(unsavedBlokkade.getStartTime()) + " - " + DateUtil.formatTime(unsavedBlokkade.getEndTime());
			}
		}
		return periodeTekst;
	}

	@Override
	public List<ColonTijdslotDto> zoekBlokkades(RoosterListViewFilter filter, long intakelocatieId)
	{
		return roosterService.searchTijdslots(filter, intakelocatieId, ColonTijdSlotType.BLOKKADE);
	}

	@Override
	@Transactional
	public void bulkDeleteBlokkades(List<Long> blokkadeIds, InstellingGebruiker loggedInInstellingGebruiker, boolean alleenValidatie) throws BulkVerwijderenException
	{
		var exception = new BulkVerwijderenException(ColonTijdSlotType.BLOKKADE);
		var teVerwijderenBlokkades = new ArrayList<ColonBlokkade>();
		blokkadeIds.forEach(id ->
		{
			try
			{
				var dbBlokkade = blokkadeRepository.findById(id).orElse(null);
				magVerwijderen(dbBlokkade);
				teVerwijderenBlokkades.add(dbBlokkade);
				exception.aantalVerwijderenOphogen();
			}
			catch (ValidatieException e)
			{
				exception.aantalNietGevondenOphogen();
			}
		});

		if (alleenValidatie)
		{
			throw exception;
		}
		else
		{
			blokkadeRepository.deleteAll(teVerwijderenBlokkades);
			logService.logGebeurtenis(LogGebeurtenis.COLON_BLOKKADES_VERWIJDEREN, loggedInInstellingGebruiker, exception.getMessage(), Bevolkingsonderzoek.COLON);
		}
	}

}
