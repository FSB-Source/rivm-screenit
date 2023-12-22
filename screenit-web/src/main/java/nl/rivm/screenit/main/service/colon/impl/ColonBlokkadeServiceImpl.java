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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import javax.annotation.Nullable;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.exceptions.TijdBlokOverlapException;
import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.main.model.RecurrenceOption;
import nl.rivm.screenit.main.service.colon.ColonBlokkadeService;
import nl.rivm.screenit.main.service.colon.RoosterService;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.dto.ColonBlokkadeDto;
import nl.rivm.screenit.model.colon.planning.ColonBlokkade;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.repository.colon.ColonBlokkadeRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.specification.colon.ColonBlokkadeSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment_;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.NoRecurrence;

import org.apache.commons.collections.CollectionUtils;
import org.hibernate.Hibernate;
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

	private final HibernateService hibernateService;

	private final LogService logService;

	private final ColonBlokkadeRepository blokkadeRepository;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	@Transactional
	public void createBlokkade(ColonBlokkadeDto blokkadeDto, InstellingGebruiker instellingGebruiker) throws ValidatieException, OpslaanVerwijderenTijdBlokException
	{
		var intakelocatie = roosterService.getIntakelocatieVanInstellingGebruiker(instellingGebruiker);
		var dbBlokkade = new ColonBlokkade();
		var blokkade = converteerBlokkade(blokkadeDto, intakelocatie, dbBlokkade);

		valideerBlokkade(blokkade, blokkadeDto.getAlleKamers(), intakelocatie);

		var transformedBlokkades = splitBlokkade(blokkade, blokkadeDto.getAlleKamers(), intakelocatie);
		for (var transformedBlokkade : transformedBlokkades)
		{
			hibernateService.saveOrUpdate(transformedBlokkade);
		}
		logAction(blokkade, instellingGebruiker, intakelocatie, null, LogGebeurtenis.COLON_BLOKKADES_NIEUW);
	}

	private void valideerBlokkade(ColonBlokkade blokkade, boolean alleKamers, ColoscopieCentrum intakelocatie) throws ValidatieException, OpslaanVerwijderenTijdBlokException
	{
		roosterService.valideerTijdslot(blokkade);
		heeftOverlappendeBlokkades(blokkade, null, null, null, true);
		roosterService.magBlokkadeOpslaanVerwijderen(blokkade, null, null, null, false, getBlokkadeKamers(blokkade, alleKamers, intakelocatie));
	}

	private void heeftOverlappendeBlokkades(ColonBlokkade blokkade, RecurrenceOption recurrenceOption, Date recurrenceEditEnd,
		Date origRecEndDateTime, boolean wijzigen) throws OpslaanVerwijderenTijdBlokException
	{
		var currentViewRange = roosterService.getCurrentViewRange(blokkade, recurrenceOption, recurrenceEditEnd, origRecEndDateTime);

		var nieuweBlokkades = roosterService.getNieuweTijdSloten(blokkade, currentViewRange);

		List<ColonBlokkade> overlapteBlokkades = null;
		if (wijzigen || blokkade.getId() == null) 
		{
			overlapteBlokkades = getBlokkadeTijden(nieuweBlokkades, blokkade, currentViewRange);
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
			var idsVanBestaandeBlokkades = new HashSet<Long>();
			if (blokkade.getRecurrence() != null && !NoRecurrence.class.isAssignableFrom(blokkade.getRecurrence().getClass()))
			{
				for (var appointment : blokkade.getRecurrence().getAppointments())
				{
					idsVanBestaandeBlokkades.add(appointment.getId());
				}
			}
			else
			{
				idsVanBestaandeBlokkades.add(blokkade.getId());
			}

			for (var overlapteRoosterBlokItem : overlapteBlokkades)
			{
				var startDateTimeBestaand = DateUtil.startMinuut(overlapteRoosterBlokItem.getStartTime());
				var endDateTimeBestaand = DateUtil.startMinuut(overlapteRoosterBlokItem.getEndTime());
				var overlapteRoosterBlok = Range.closed(startDateTimeBestaand, endDateTimeBestaand);

				if (!idsVanBestaandeBlokkades.contains(overlapteRoosterBlokItem.getId()))
				{

					echteOverlapteBlokkades.add(overlapteRoosterBlok);
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
	public List<ColonBlokkade> splitBlokkade(ColonBlokkade unsavedObject, boolean alleKamers, ColoscopieCentrum intakelocatie)
	{
		var list = new ArrayList<ColonBlokkade>();
		if (unsavedObject.getLocation() != null)
		{
			list.add(unsavedObject);
		}
		if (alleKamers)
		{
			for (var kamer : getActieveKamers(intakelocatie))
			{
				if (!Objects.equals(kamer, unsavedObject.getLocation()))
				{
					var blokkade = unsavedObject.transientClone();
					var recurrence = unsavedObject.getRecurrence();
					if (recurrence != null && !NoRecurrence.class.isAssignableFrom(Hibernate.getClass(recurrence)))
					{
						var clonedRecurrence = recurrence.transientClone();
						clonedRecurrence.setFirstAppointment(blokkade);
						blokkade.setRecurrence(clonedRecurrence);
					}
					blokkade.setLocation(kamer);
					list.add(blokkade);
				}
			}
		}
		return list;
	}

	@Override
	@Transactional
	public void deleteBlokkade(Long blokkadeId, InstellingGebruiker instellingGebruiker) throws ValidatieException
	{
		var blokkade = getBlokkade(blokkadeId).orElse(null);
		var intakelocatie = roosterService.getIntakelocatieVanInstellingGebruiker(instellingGebruiker);

		magVerwijderen(blokkade);

		blokkadeRepository.delete(blokkade);
		logAction(blokkade, instellingGebruiker, intakelocatie, null, LogGebeurtenis.COLON_BLOKKADES_VERWIJDEREN);
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

	private static List<Kamer> getBlokkadeKamers(ColonBlokkade blokkade, boolean alleKamers, ColoscopieCentrum intakelocatie)
	{
		return alleKamers ? getActieveKamers(intakelocatie) : Arrays.asList(blokkade.getLocation());
	}

	private List<ColonBlokkade> getBlokkadeTijden(List<Range<Date>> ranges, ColonBlokkade blokkade, Range<Date> totaalInterval)
	{
		return blokkadeRepository.findAll(ColonBlokkadeSpecification.valtBinnenRanges(ranges)
				.and(ColonBlokkadeSpecification.heeftKamer(blokkade.getLocation()))
				.and(ColonBlokkadeSpecification.valtBinnenRecurrence(blokkade, totaalInterval))
			, Sort.by(Sort.Direction.ASC, AbstractAppointment_.START_TIME));
	}

	private ColonBlokkade converteerBlokkade(ColonBlokkadeDto blokkadeDto, ColoscopieCentrum intakelocatie, ColonBlokkade dbBlokkade)
	{
		dbBlokkade.setStartTime(DateUtil.toUtilDate(blokkadeDto.getStartTime()));
		dbBlokkade.setEndTime(DateUtil.toUtilDate(blokkadeDto.getEndTime()));
		var kamer = intakelocatie.getKamers().stream().filter(k -> k.getId().equals(blokkadeDto.getKamerId())).findFirst()
			.orElse(null);
		dbBlokkade.setLocation(kamer);
		dbBlokkade.setDescription(blokkadeDto.getDescription());

		return dbBlokkade;
	}

	@Override
	@Transactional
	public void logAction(ColonBlokkade unsavedObject, InstellingGebruiker instellingGebruiker, ColoscopieCentrum intakelocatie, @Nullable ColonBlokkade origineleBlokkade,
		LogGebeurtenis logGebeurtenis)
	{
		var selectedKamer = unsavedObject.getLocation();
		if (selectedKamer != null)
		{
			logActionKamer(unsavedObject, instellingGebruiker, selectedKamer, origineleBlokkade, logGebeurtenis);
		}
		else
		{
			for (Kamer kamer : getActieveKamers(intakelocatie))
			{
				logActionKamer(unsavedObject, instellingGebruiker, kamer, origineleBlokkade, logGebeurtenis);
			}
		}
	}

	private void logActionKamer(ColonBlokkade unsavedObject, InstellingGebruiker instellingGebruiker, Kamer kamer, @Nullable ColonBlokkade origineleBlokkade,
		LogGebeurtenis logGebeurtenis)
	{
		var melding = getPeriodeTekst(unsavedObject) + ", " + kamer.getName() + ", " + kamer.getColoscopieCentrum().getNaam();
		if (unsavedObject.getRecurrence() != null && !NoRecurrence.class.isAssignableFrom(unsavedObject.getRecurrence().getClass()))
		{
			melding += ", in " + unsavedObject.getRecurrence().getName() + " reeks ";
			if (unsavedObject.getRecurrence().getEndDate() != null)
			{
				melding += " t/m " + DateUtil.formatShortDate(unsavedObject.getRecurrence().getEndDate());
			}
		}

		if (logGebeurtenis == LogGebeurtenis.COLON_BLOKKADES_WIJZIG && origineleBlokkade != null)
		{
			melding = DateUtil.formatShortDateTime(origineleBlokkade.getStartTime()) + " -> " + melding;
		}
		logService.logGebeurtenis(logGebeurtenis, instellingGebruiker, melding, Bevolkingsonderzoek.COLON);
	}

	@Override
	public String getPeriodeTekst(ColonBlokkade unsavedObject)
	{
		var periodeTekst = "";
		if (unsavedObject != null && unsavedObject.getStartTime() != null && unsavedObject.getEndTime() != null)
		{
			periodeTekst = DateUtil.formatShortDate(unsavedObject.getStartTime()) + " ";
			if (!DateUtil.isZelfdeDag(unsavedObject.getEndTime(), unsavedObject.getStartTime()))
			{
				periodeTekst += "hele dag";
			}
			else
			{
				periodeTekst += DateUtil.formatTime(unsavedObject.getStartTime()) + " - " + DateUtil.formatTime(unsavedObject.getEndTime());
			}
		}
		return periodeTekst;
	}
}
