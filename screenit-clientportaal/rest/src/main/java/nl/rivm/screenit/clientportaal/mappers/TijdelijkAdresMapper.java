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

import nl.rivm.screenit.clientportaal.model.TijdelijkAdresDto;
import nl.rivm.screenit.mappers.config.ScreenitMapperConfig;
import nl.rivm.screenit.model.TijdelijkAdres;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.MappingTarget;
import org.mapstruct.Mappings;
import org.mapstruct.Named;

@Mapper(config = ScreenitMapperConfig.class)
public interface TijdelijkAdresMapper
{
	@Mappings({
		@Mapping(source = "huisnummerToevoeging", target = "huisnummerToevoeging", qualifiedByName = "conversionNullToBlankString"),
		@Mapping(source = "huisletter", target = "huisletter", qualifiedByName = "conversionNullToBlankString"),
		@Mapping(source = "huisnummerAanduiding", target = "huisnummerAanduiding", qualifiedByName = "conversionNullToBlankString")
	})
	TijdelijkAdresDto tijdelijkAdresToDto(TijdelijkAdres tijdelijkAdres);

	@Mappings({
		@Mapping(source = "postcode", target = "postcode", qualifiedByName = "conversionPostcode"),
		@Mapping(target = "soort", ignore = true),
		@Mapping(target = "geheimadres", ignore = true),
		@Mapping(target = "tijdelijk", ignore = true),
		@Mapping(target = "gemeente", ignore = true),
		@Mapping(target = "gemeentedeel", ignore = true),
		@Mapping(target = "land", ignore = true),
		@Mapping(target = "aanschrijfAdres", ignore = true),
		@Mapping(target = "gemeenteCode", ignore = true),
		@Mapping(target = "locatieBeschrijving", ignore = true),
		@Mapping(target = "postcodeCoordinaten", ignore = true)
	})
	TijdelijkAdres dtoToTijdelijkAdres(TijdelijkAdresDto tijdelijkAdresDto);

	@Named("conversionPostcode")
	default String conversionPostcode(String postcode)
	{
		return postcode.toUpperCase();
	}

	@Named("conversionNullToBlankString")
	default String conversionNullToBlankString(String string)
	{
		return string == null ? "" : string;
	}

	TijdelijkAdres updateTijdelijkAdres(@MappingTarget TijdelijkAdres target, TijdelijkAdres source);
}
