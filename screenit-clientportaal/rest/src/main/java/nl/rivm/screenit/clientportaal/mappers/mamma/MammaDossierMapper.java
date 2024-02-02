package nl.rivm.screenit.clientportaal.mappers.mamma;

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

import java.util.Date;
import java.util.List;

import nl.rivm.screenit.clientportaal.mappers.BaseDossierMapper;
import nl.rivm.screenit.clientportaal.mappers.ClientGebeurtenisMapper;
import nl.rivm.screenit.clientportaal.model.mamma.MammaDossierDto;
import nl.rivm.screenit.mappers.config.ScreenitMapperConfig;
import nl.rivm.screenit.model.ClientGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.topicuszorg.hibernate.spring.util.ApplicationContextProvider;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.Named;
import org.mapstruct.ReportingPolicy;

@Mapper(config = ScreenitMapperConfig.class, uses = { ClientGebeurtenisMapper.class }, unmappedTargetPolicy = ReportingPolicy.WARN)
public interface MammaDossierMapper extends BaseDossierMapper<MammaDossier, MammaDossierDto>
{
	@Override
	@Mappings({
		@Mapping(source = "dossier", target = "laatsteStandplaatsPlaats", qualifiedByName = "dossierToLaatsteStandplaatsPlaats"),
		@Mapping(source = "clientGebeurtenisList", target = "gebeurtenissenLaatsteRonde")
	})
	MammaDossierDto mapToDto(MammaDossier dossier, List<ClientGebeurtenis> clientGebeurtenisList);

	@Named("dossierToLaatsteStandplaatsPlaats")
	static String dossierToLaatsteStandplaatsPlaats(MammaDossier dossier)
	{
		MammaBaseStandplaatsService baseStandplaatsService = ApplicationContextProvider.getApplicationContext().getBean(MammaBaseStandplaatsService.class);
		MammaUitnodiging laatsteUitnodiging = dossier.getLaatsteScreeningRonde() != null ? dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging() : null;
		String plaats = null;
		if (laatsteUitnodiging != null)
		{
			MammaStandplaats standplaats = laatsteUitnodiging.getLaatsteAfspraak() != null
				? laatsteUitnodiging.getLaatsteAfspraak().getStandplaatsPeriode().getStandplaatsRonde().getStandplaats()
				: laatsteUitnodiging.getStandplaatsRonde().getStandplaats();
			plaats = baseStandplaatsService.getStandplaatsLocatie(standplaats, new Date()).getPlaats();
		}
		return plaats;
	}
}
