package nl.rivm.screenit.clientportaal.mappers.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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

import nl.rivm.screenit.clientportaal.model.mamma.MammaAfspraakWijzigenFilterDto;
import nl.rivm.screenit.clientportaal.model.mamma.MammaAfspraakZoekFilterDto;
import nl.rivm.screenit.mappers.config.ScreenitMapperConfig;
import nl.rivm.screenit.model.Client;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;

@Mapper(config = ScreenitMapperConfig.class)
public interface MammaAfspraakZoekFilterMapper
{

	@Mappings({
		@Mapping(source = "afspraakZoekFilterDto.vanaf", target = "vanaf"),
		@Mapping(source = "afspraakZoekFilterDto.vanaf", target = "totEnMet"),
		@Mapping(source = "afspraakZoekFilterDto.meerOpties", target = "extraOpties"),
		@Mapping(source = "client", target = "client"),
		@Mapping(target = "standplaatsen", ignore = true),
		@Mapping(target = "screeningsEenheden", ignore = true),
		@Mapping(target = "buitenRegio", ignore = true),
		@Mapping(target = "verzettenReden", ignore = true),
	})
	MammaAfspraakWijzigenFilterDto afspraakZoekFilterToAfspraakWijzigenFilterDto(MammaAfspraakZoekFilterDto afspraakZoekFilterDto, Client client);
}
