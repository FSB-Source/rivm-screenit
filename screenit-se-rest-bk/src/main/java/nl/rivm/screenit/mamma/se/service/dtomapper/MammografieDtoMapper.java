package nl.rivm.screenit.mamma.se.service.dtomapper;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.mamma.se.dto.onderzoek.AnnotatieAfbeeldingSeDto;
import nl.rivm.screenit.mamma.se.dto.onderzoek.MammografieSeDto;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaAnnotatieAfbeelding;
import nl.rivm.screenit.model.mamma.MammaMammografie;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;

public class MammografieDtoMapper
{
	public MammografieSeDto createMammografieDto(MammaAfspraak afspraak)
	{
		MammaOnderzoek onderzoek = afspraak.getOnderzoek();
		return onderzoek != null && onderzoek.getMammografie() != null ? createMammografieDto(onderzoek.getMammografie()) : null;
	}

	private MammografieSeDto createMammografieDto(MammaMammografie mammografie)
	{
		MammografieSeDto mammografieSeDto = new MammografieSeDto();
		mammografieSeDto.setId(mammografie.getId());
		mammografieSeDto.setVisueleInspectieAfbeelding(createAfbeeldingDto(mammografie.getVisueleInspectieAfbeelding()));
		return mammografieSeDto;
	}

	private AnnotatieAfbeeldingSeDto createAfbeeldingDto(MammaAnnotatieAfbeelding afbeelding)
	{
		return new AfbeeldingDtoMapper().createAfbeeldingDto(afbeelding);
	}

}
