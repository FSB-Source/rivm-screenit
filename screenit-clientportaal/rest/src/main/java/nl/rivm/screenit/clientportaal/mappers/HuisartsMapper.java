package nl.rivm.screenit.clientportaal.mappers;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.clientportaal.model.HuisartsDto;
import nl.rivm.screenit.clientportaal.model.HuisartsZoekDto;
import nl.rivm.screenit.mappers.config.ScreenitMapperConfig;
import nl.rivm.screenit.model.EnovationHuisarts;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;

@Mapper(config = ScreenitMapperConfig.class, uses = { AdresMapper.class })
public interface HuisartsMapper
{
	@Mappings({
		@Mapping(source = "tussenvoegels", target = "tussenvoegsels"),
	})
	HuisartsDto huisartsToDto(EnovationHuisarts huisarts);

	@Mappings({
		@Mapping(source = "naam", target = "achternaam"),
		@Mapping(target = "id", ignore = true),
		@Mapping(target = "voorletters", ignore = true),
		@Mapping(target = "tussenvoegels", ignore = true),
		@Mapping(target = "praktijknaam", ignore = true),
		@Mapping(target = "ediadres", ignore = true),
		@Mapping(target = "gewijzigd", ignore = true),
		@Mapping(target = "klantnummer", ignore = true),
		@Mapping(target = "geslacht", ignore = true),
		@Mapping(target = "telefoonnummer", ignore = true),
		@Mapping(target = "verwijderd", ignore = true),
		@Mapping(target = "huisartsAgb", ignore = true),
		@Mapping(target = "praktijkAgb", ignore = true),
		@Mapping(target = "weergavenaam", ignore = true),
		@Mapping(target = "oorspronkelijkEdiadres", ignore = true),
	})
	EnovationHuisarts zoekDtoToHuisarts(HuisartsZoekDto huisartsZoekDto);

	List<HuisartsDto> huisartsenToDtos(List<EnovationHuisarts> huisartsen);
}
