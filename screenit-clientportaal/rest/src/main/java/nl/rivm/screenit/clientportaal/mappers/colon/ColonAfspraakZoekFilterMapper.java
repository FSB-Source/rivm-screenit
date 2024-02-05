package nl.rivm.screenit.clientportaal.mappers.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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

import nl.rivm.screenit.clientportaal.model.colon.ColonAfspraakZoekFilterDto;
import nl.rivm.screenit.mappers.config.ScreenitMapperConfig;
import nl.rivm.screenit.model.colon.planning.VrijSlotZonderKamerFilter;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;

@Mapper(config = ScreenitMapperConfig.class)
public interface ColonAfspraakZoekFilterMapper
{

	@Mappings({
		@Mapping(source = "ziekenhuisnaam", target = "naam"),
		@Mapping(target = "intakeLocatieId", ignore = true),
		@Mapping(target = "nietIntakeLocatieId", ignore = true),
		@Mapping(target = "alleenIntakeLokaties", ignore = true),
	})
	VrijSlotZonderKamerFilter vrijSlotToColonVrijSlotZonderKamerDto(ColonAfspraakZoekFilterDto colonAfspraakZoekFilterDto);

}
