package nl.rivm.screenit.mamma.se.service.dtomapper;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import nl.rivm.screenit.mamma.se.dto.onderzoek.AnnotatieAfbeeldingSeDto;
import nl.rivm.screenit.mamma.se.dto.onderzoek.AnnotatieIcoonSeDto;
import nl.rivm.screenit.mamma.se.dto.onderzoek.DoorsnedeAfbeeldingenSeDto;
import nl.rivm.screenit.model.mamma.MammaAnnotatieAfbeelding;
import nl.rivm.screenit.model.mamma.MammaLaesie;
import nl.rivm.screenit.model.mamma.MammaLaesieIcoon;
import nl.rivm.screenit.model.mamma.enums.MammaAnnotatieIcoonType;
import nl.rivm.screenit.model.mamma.enums.MammaLaesieType;
import nl.rivm.screenit.model.mamma.enums.MammaZijde;

public class DoorsnedeAfbeeldingenDtoMapper
{
	private AfbeeldingDtoMapper mapper = new AfbeeldingDtoMapper();

	public DoorsnedeAfbeeldingenSeDto createDoorsnedeAfbeeldingenDtoFromLaesies(List<MammaLaesie> laesies)
	{
		DoorsnedeAfbeeldingenSeDto doorsnedeAfbeeldingenDto = new DoorsnedeAfbeeldingenSeDto();
		doorsnedeAfbeeldingenDto.setRechtsHorizontaleDoorsnede(mapToAfbeelding(getHorizontaleAnnotatieIcoonCollection(laesies, MammaZijde.RECHTER_BORST)));
		doorsnedeAfbeeldingenDto.setLinksHorizontaleDoorsnede(mapToAfbeelding(getHorizontaleAnnotatieIcoonCollection(laesies, MammaZijde.LINKER_BORST)));
		doorsnedeAfbeeldingenDto.setRechtsVerticaleDoorsnede(mapToAfbeelding(getVerticaleAnnotatieIcoonCollection(laesies, MammaZijde.RECHTER_BORST)));
		doorsnedeAfbeeldingenDto.setLinksVerticaleDoorsnede(mapToAfbeelding(getVerticaleAnnotatieIcoonCollection(laesies, MammaZijde.LINKER_BORST)));
		return doorsnedeAfbeeldingenDto;
	}

	public DoorsnedeAfbeeldingenSeDto createDoorsnedeAfbeeldingenDtoFromAanzichten(MammaAnnotatieAfbeelding rechtsVerticaleDoorsnede,
		MammaAnnotatieAfbeelding linksVerticaleDoorsnede, MammaAnnotatieAfbeelding rechtsHorizontaleDoorsnede, MammaAnnotatieAfbeelding linksHorizontaleDoorsnede)
	{
		DoorsnedeAfbeeldingenSeDto doorsnedeAfbeeldingenDto = new DoorsnedeAfbeeldingenSeDto();
		doorsnedeAfbeeldingenDto.setRechtsHorizontaleDoorsnede(mapper.createAfbeeldingDto(rechtsHorizontaleDoorsnede));
		doorsnedeAfbeeldingenDto.setLinksHorizontaleDoorsnede(mapper.createAfbeeldingDto(linksHorizontaleDoorsnede));
		doorsnedeAfbeeldingenDto.setRechtsVerticaleDoorsnede(mapper.createAfbeeldingDto(rechtsVerticaleDoorsnede));
		doorsnedeAfbeeldingenDto.setLinksVerticaleDoorsnede(mapper.createAfbeeldingDto(linksVerticaleDoorsnede));
		return doorsnedeAfbeeldingenDto;
	}

	private List<AnnotatieIcoonSeDto> getHorizontaleAnnotatieIcoonCollection(List<MammaLaesie> laesies, MammaZijde zijde)
	{
		return laesies.stream().filter(laesie -> laesie.getMammaZijde().equals(zijde))
			.map(laesie -> mapLaesieIcoonToIcoonSeDto(laesie.getHorizontaleDoorsnedeIcoon(), laesie.getMammaLaesieType())).filter(Objects::nonNull).collect(Collectors.toList());
	}

	private List<AnnotatieIcoonSeDto> getVerticaleAnnotatieIcoonCollection(List<MammaLaesie> laesies, MammaZijde zijde)
	{
		return laesies.stream().filter(laesie -> laesie.getMammaZijde().equals(zijde))
			.map(laesie -> mapLaesieIcoonToIcoonSeDto(laesie.getVerticaleDoorsnedeIcoon(), laesie.getMammaLaesieType())).filter(Objects::nonNull).collect(Collectors.toList());
	}

	private AnnotatieAfbeeldingSeDto mapToAfbeelding(List<AnnotatieIcoonSeDto> iconen)
	{
		AnnotatieAfbeeldingSeDto afbeeldingSeDto = new AnnotatieAfbeeldingSeDto();
		afbeeldingSeDto.setIconen(iconen);
		return afbeeldingSeDto;
	}

	private AnnotatieIcoonSeDto mapLaesieIcoonToIcoonSeDto(MammaLaesieIcoon laesieicoon, MammaLaesieType type)
	{
		if (laesieicoon != null)
		{
			AnnotatieIcoonSeDto dto = new AnnotatieIcoonSeDto();
			dto.setType(mapAnnotatieIcoonType(type));
			dto.setPositieX(laesieicoon.getPositieX().doubleValue());
			dto.setPositieY(laesieicoon.getPositieY().doubleValue());
			return dto;
		}
		else
		{
			return null;
		}
	}

	private MammaAnnotatieIcoonType mapAnnotatieIcoonType(MammaLaesieType type)
	{
		switch (type)
		{
		case ARCHITECTUURVERSTORING:
			return MammaAnnotatieIcoonType.SIGNALERING_ARCHITECTUURVERSTORING;
		case ASYMMETRIE:
			return MammaAnnotatieIcoonType.SIGNALERING_ASYMMETRIE;
		case CALCIFICATIES:
			return MammaAnnotatieIcoonType.SIGNALERING_CALCIFICATIES;
		case MASSA:
			return MammaAnnotatieIcoonType.SIGNALERING_MASSA;
		case LEGACY_ARCHITECTUURVERSTORING_MET_CALCIFICATIES:
			return MammaAnnotatieIcoonType.LEGACY_ARCHITECTUURVERSTORING_MET_CALCIFICATIES;
		case LEGACY_MASSA_MET_ARCHITECTUURVERSTORING:
			return MammaAnnotatieIcoonType.LEGACY_MASSA_MET_ARCHITECTUURVERSTORING;
		case LEGACY_CONFORM:
			return MammaAnnotatieIcoonType.LEGACY_CONFORM;
		case LEGACY_GEEN_BIJZONDERHEDEN:
			return MammaAnnotatieIcoonType.LEGACY_GEEN_BIJZONDERHEDEN;
		case LEGACY_MASSA_MET_SPICULAE:
			return MammaAnnotatieIcoonType.LEGACY_MASSA_MET_SPICULAE;
		case LEGACY_PROJECTIE_NAAR_LINKS:
			return MammaAnnotatieIcoonType.LEGACY_PROJECTIE_NAAR_LINKS;
		case LEGACY_PROJECTIE_NAAR_RECHTS:
			return MammaAnnotatieIcoonType.LEGACY_PROJECTIE_NAAR_RECHTS;
		case LEGACY_MASSA_MET_CALCIFICATIES:
			return MammaAnnotatieIcoonType.LEGACY_MASSA_MET_CALCIFICATIES;
		case LEGACY_MASSA_MET_SPICULAE_EN_CALCIFICATIES:
			return MammaAnnotatieIcoonType.LEGACY_MASSA_MET_SPICULAE_EN_CALCIFICATIES;
		case LEGACY_BENIGNE_KALK:
			return MammaAnnotatieIcoonType.LEGACY_BENIGNE_KALK;
		case LEGACY_MARKERING:
			return MammaAnnotatieIcoonType.LEGACY_MARKERING;
		default:
			throw new IllegalArgumentException("Niet bestaand MammaLaesieType");
		}
	}
}
