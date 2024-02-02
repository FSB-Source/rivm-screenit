package nl.rivm.screenit.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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
import java.time.temporal.TemporalAdjusters;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.repository.colon.ColonIntakelocatieRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.colon.ColonRoosterService;
import nl.rivm.screenit.specification.colon.ColonIntakelocatieSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import com.google.common.collect.Range;

@Service
@AllArgsConstructor
public class ColonRoosterServiceImpl implements ColonRoosterService
{
	private final ColonIntakelocatieRepository intakelocatieRepository;

	private final SimplePreferenceService preferenceService;

	private final ICurrentDateSupplier dateSupplier;

	@Override
	public Range<LocalDate> getSignaleringstermijnBereik()
	{
		var signaleringsTermijn = preferenceService.getInteger(PreferenceKey.COLON_SIGNALERINGSTERMIJN_GEEN_CAPACITEIT.name(), 8);

		var start = dateSupplier.getLocalDate().plusWeeks(signaleringsTermijn);

		if (!start.getDayOfWeek().equals(DayOfWeek.MONDAY))
		{
			start = start.with(TemporalAdjusters.previous(DayOfWeek.MONDAY));
		}

		return Range.closedOpen(start, start.plusDays(6));
	}

	@Override
	public LocalDate getSignaleringstermijnDeadline()
	{
		var start = dateSupplier.getLocalDate();
		if (!start.getDayOfWeek().equals(DayOfWeek.MONDAY))
		{
			start = start.with(TemporalAdjusters.next(DayOfWeek.MONDAY));
		}
		return start;
	}

	@Override
	public String getSignaleringstermijnTekst()
	{
		var signaleringstermijn = getSignaleringstermijnBereik();
		return signaleringstermijn.lowerEndpoint().format(DateUtil.LOCAL_DATE_FORMAT) + " - " + signaleringstermijn.upperEndpoint().format(DateUtil.LOCAL_DATE_FORMAT);
	}

	@Override
	public boolean intakelocatieHeeftGeenCapaciteit(ColoscopieCentrum intakelocatie)
	{
		var results = intakelocatieRepository.findFirst(
			ColonIntakelocatieSpecification.heeftGeenCapaciteitBinnenDatum(getSignaleringstermijnBereik())
				.and(ColonIntakelocatieSpecification.isActief())
				.and(ColonIntakelocatieSpecification.isIntakelocatie(intakelocatie)), Sort.unsorted());
		return results.isPresent();
	}

}
