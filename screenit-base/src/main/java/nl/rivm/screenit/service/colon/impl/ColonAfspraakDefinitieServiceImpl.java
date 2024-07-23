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

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.planning.AfspraakDefinitie;
import nl.rivm.screenit.repository.colon.ColonAfspraakDefinitieRepository;
import nl.rivm.screenit.service.colon.ColonAfspraakDefinitieService;
import nl.rivm.screenit.specification.colon.ColonAfspraakDefinitieSpecification;

import org.springframework.stereotype.Service;

@Service
@Slf4j
@AllArgsConstructor
public class ColonAfspraakDefinitieServiceImpl implements ColonAfspraakDefinitieService
{

	private final ColonAfspraakDefinitieRepository colonAfspraakDefinitieRepository;

	@Override
	public AfspraakDefinitie getActiefAfspraakDefinitie(ColoscopieCentrum intakelocatie)
	{
		return colonAfspraakDefinitieRepository.findOne(
			ColonAfspraakDefinitieSpecification.isActief()
				.and(ColonAfspraakDefinitieSpecification.isInstelling(intakelocatie)))
			.orElseThrow(() -> new IllegalArgumentException("Geen of te veel afspraakDefinities in IL " + intakelocatie.getNaam()));
	}
}
