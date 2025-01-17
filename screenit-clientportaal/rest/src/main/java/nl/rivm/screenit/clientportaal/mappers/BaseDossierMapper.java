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

import nl.rivm.screenit.clientportaal.model.BaseDossierDto;
import nl.rivm.screenit.model.ClientGebeurtenis;
import nl.rivm.screenit.model.Dossier;

import org.mapstruct.InheritConfiguration;
import org.mapstruct.Mapping;
import org.mapstruct.MappingTarget;
import org.mapstruct.Mappings;

public interface BaseDossierMapper<SOURCE extends Dossier<?, ?>, DTO extends BaseDossierDto>
{

	@Mappings({ @Mapping(source = "clientGebeurtenisList", target = "gebeurtenissenLaatsteRonde") })
	DTO mapToDto(SOURCE pojo, List<ClientGebeurtenis> clientGebeurtenisList);

	@InheritConfiguration
	DTO updateDto(@MappingTarget DTO dto, SOURCE dossier, List<ClientGebeurtenis> clientGebeurtenisList);

	SOURCE mapToEntity(DTO dto);

	SOURCE updateEntity(@MappingTarget SOURCE dossier, DTO dto);

}
