package nl.rivm.screenit.service.mamma.impl;

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

import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode_;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde_;
import nl.rivm.screenit.repository.mamma.MammaStandplaatsRondeRepository;
import nl.rivm.screenit.service.mamma.MammaStandplaatsRondeService;

import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.specification.mamma.MammaStandplaatsRondeSpecification.heeftEersteStandplaatsPeriodeVoor;

@Service
@AllArgsConstructor
public class MammaBaseStandplaatsRondeServiceImpl implements MammaStandplaatsRondeService
{
	private MammaStandplaatsRondeRepository standplaatsRondeRepository;

	@Override
	public MammaStandplaatsRonde getVorigeStandplaatsRonde(MammaStandplaatsRonde standplaatsRonde)
	{
		var vanaf = standplaatsRonde.getStandplaatsPerioden().stream()
			.filter(standplaatsPeriode -> standplaatsPeriode.getStandplaatsRondeVolgNr() == 1).findFirst()
			.orElseThrow()
			.getVanaf();

		var vorigeStandplaats = standplaatsRondeRepository.findFirst(heeftEersteStandplaatsPeriodeVoor(vanaf),
			Sort.by(MammaStandplaatsRonde_.STANDPLAATS_PERIODEN + "." + MammaStandplaatsPeriode_.VANAF).descending());

		return vorigeStandplaats.orElse(null);
	}
}
