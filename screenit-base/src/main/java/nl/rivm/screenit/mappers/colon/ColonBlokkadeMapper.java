package nl.rivm.screenit.mappers.colon;

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

import nl.rivm.screenit.mappers.config.ScreenitMapperConfig;
import nl.rivm.screenit.model.colon.dto.ColonBlokkadeDto;
import nl.rivm.screenit.model.colon.planning.ColonBlokkade;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper(config = ScreenitMapperConfig.class)
public interface ColonBlokkadeMapper
{
	@Mapping(source = "id", target = "id")
	@Mapping(source = "location.name", target = "kamer")
	@Mapping(source = "location.id", target = "kamerId")
	@Mapping(source = "startTime", target = "startTime")
	@Mapping(source = "endTime", target = "endTime")
	@Mapping(source = "description", target = "description")
	@Mapping(target = "alleKamers", ignore = true)
	@Mapping(target = "alleenValidatie", ignore = true)
	@Mapping(target = "herhaling", ignore = true)
	ColonBlokkadeDto colonBlokkadeToDto(ColonBlokkade colonBlokkade);
}
