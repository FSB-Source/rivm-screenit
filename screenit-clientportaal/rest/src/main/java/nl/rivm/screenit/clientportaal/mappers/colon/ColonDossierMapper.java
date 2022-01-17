package nl.rivm.screenit.clientportaal.mappers.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.clientportaal.mappers.BaseDossierMapper;
import nl.rivm.screenit.clientportaal.mappers.ClientGebeurtenisMapper;
import nl.rivm.screenit.clientportaal.mappers.config.ScreenitMapperConfig;
import nl.rivm.screenit.clientportaal.model.colon.ColonDossierDto;
import nl.rivm.screenit.model.colon.ColonDossier;

import org.mapstruct.Mapper;
import org.mapstruct.ReportingPolicy;

@Mapper(config = ScreenitMapperConfig.class, uses = { ClientGebeurtenisMapper.class }, unmappedTargetPolicy = ReportingPolicy.WARN)
public interface ColonDossierMapper extends BaseDossierMapper<ColonDossier, ColonDossierDto>
{

}
