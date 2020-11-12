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

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.stream.Collectors;

import nl.rivm.screenit.mamma.se.dto.onderzoek.AnnotatieAfbeeldingSeDto;
import nl.rivm.screenit.mamma.se.dto.onderzoek.AnnotatieIcoonSeDto;
import nl.rivm.screenit.model.mamma.MammaAnnotatieAfbeelding;
import nl.rivm.screenit.model.mamma.MammaAnnotatieIcoon;

public class AfbeeldingDtoMapper
{
	AnnotatieAfbeeldingSeDto createAfbeeldingDto(MammaAnnotatieAfbeelding afbeelding)
	{
		AnnotatieAfbeeldingSeDto afbeeldingDto = null;
		if (afbeelding != null)
		{
			afbeeldingDto = new AnnotatieAfbeeldingSeDto();
			afbeeldingDto.setId(afbeelding.getId());
			afbeeldingDto.setIconen(afbeelding.getIconen().stream().map(this::createIcoonDto).collect(Collectors.toList()));
		}
		return afbeeldingDto;
	}

	private AnnotatieIcoonSeDto createIcoonDto(MammaAnnotatieIcoon icoon)
	{
		AnnotatieIcoonSeDto result = new AnnotatieIcoonSeDto();
		result.setId(icoon.getId());
		result.setPositieX(icoon.getPositieX().doubleValue());
		result.setPositieY(icoon.getPositieY().doubleValue());
		result.setType(icoon.getType());
		result.setTekst(icoon.getTekst());
		return result;
	}

	public MammaAnnotatieIcoon icoonDtoToAnnotatieIcoon(AnnotatieIcoonSeDto icoonSeDto)
	{
		MammaAnnotatieIcoon result = new MammaAnnotatieIcoon();
		result.setId(icoonSeDto.getId());
		BigDecimal posX = new BigDecimal(icoonSeDto.getPositieX());
		result.setPositieX(posX.setScale(3, RoundingMode.HALF_UP));
		BigDecimal posY = new BigDecimal(icoonSeDto.getPositieY());
		result.setPositieY(posY.setScale(3, RoundingMode.HALF_UP));
		result.setType(icoonSeDto.getType());
		result.setTekst(icoonSeDto.getTekst());
		return result;
	}

}
