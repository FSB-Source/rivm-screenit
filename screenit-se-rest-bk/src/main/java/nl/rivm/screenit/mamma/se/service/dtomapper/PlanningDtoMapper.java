package nl.rivm.screenit.mamma.se.service.dtomapper;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.mamma.se.dto.GeenScreeningBlokSeDto;
import nl.rivm.screenit.mamma.se.dto.PlanningSeDto;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.util.DateUtil;

public class PlanningDtoMapper
{

	public PlanningSeDto createPlanningSeDto(List<MammaCapaciteitBlok> blokken)
	{
		PlanningSeDto planningSeDto = new PlanningSeDto();

		planningSeDto.setGeenScreeningBlokken(blokken.stream().map(this::createGeenScreeningBlokSeDto).collect(Collectors.toList()));

		return planningSeDto;
	}

	private GeenScreeningBlokSeDto createGeenScreeningBlokSeDto(MammaCapaciteitBlok blok)
	{
		GeenScreeningBlokSeDto geenScreeningBlokSeDto = new GeenScreeningBlokSeDto();
		geenScreeningBlokSeDto.setVanaf(DateUtil.toLocalDateTime(blok.getVanaf()));
		geenScreeningBlokSeDto.setTot(DateUtil.toLocalDateTime(blok.getTot()));
		geenScreeningBlokSeDto.setOpmerking(blok.getOpmerkingen());

		return geenScreeningBlokSeDto;
	}
}
