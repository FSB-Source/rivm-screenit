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
import java.time.LocalTime;
import java.util.List;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.main.service.colon.ColonIntakeAfspraakService;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.WerklijstIntakeFilter;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.repository.colon.ColonIntakeAfspraakRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.specification.colon.ColonIntakeAfspraakSpecification;

import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import com.google.common.primitives.Ints;

@Service
@AllArgsConstructor
public class ColonIntakeAfspraakServiceImpl implements ColonIntakeAfspraakService
{
	private final ColonIntakeAfspraakRepository intakeAfspraakRepository;

	private final ICurrentDateSupplier currentDateSupplier;

	private Specification<ColonIntakeAfspraak> getSpecification(ColoscopieCentrum intakelocatie, WerklijstIntakeFilter zoekObject, ICurrentDateSupplier dateSupplier)
	{
		return ColonIntakeAfspraakSpecification.heeftGeenVerslagen()
			.and(ColonIntakeAfspraakSpecification.heeftClientNietOverledenOfVerhuisdVoorColoscopie())
			.and(ColonIntakeAfspraakSpecification.heeftConclusieType(ColonConclusieType.COLOSCOPIE))
			.and(ColonIntakeAfspraakSpecification.heeftIntakelocatie(intakelocatie))
			.and(ColonIntakeAfspraakSpecification.heeftConclusieInVerleden(dateSupplier))

			.and(ColonIntakeAfspraakSpecification.metFilter(zoekObject));
	}

	@Override
	public List<ColonIntakeAfspraak> getAfsprakenZonderVerslag(WerklijstIntakeFilter zoekObject, ColoscopieCentrum intakeLocatie, long start, long size, String sortProperty,
		boolean isAscending)
	{
		var page = Double.valueOf(Math.floor((double) start / (double) size));

		Pageable pageable = PageRequest.of(page.intValue(), Ints.checkedCast(size), isAscending ? Sort.Direction.ASC : Sort.Direction.DESC, sortProperty);

		return intakeAfspraakRepository.findAll(getSpecification(intakeLocatie, zoekObject, currentDateSupplier), pageable).toList();
	}

	@Override
	public long getAantalAfsprakenZonderVerslag(WerklijstIntakeFilter zoekObject, ColoscopieCentrum intakeLocatie)
	{
		return intakeAfspraakRepository.count(getSpecification(intakeLocatie, zoekObject, currentDateSupplier));
	}

	@Override
	public long countAfsprakenOpDagVanDeWeek(DayOfWeek dagVanDeWeek)
	{
		var result = intakeAfspraakRepository.countColonIntakeAfsprakenOpDag(dagVanDeWeek.getValue());
		return result.get(0);
	}

	@Override
	public long countAfsprakenInNacht(LocalTime startTijd, LocalTime eindTijd)
	{
		var result = intakeAfspraakRepository.countColonIntakeAfsprakenInNacht(eindTijd, startTijd);
		return result.get(0);
	}
}
