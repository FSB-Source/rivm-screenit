package nl.rivm.screenit.mappers.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.time.LocalDateTime;
import java.util.Date;

import nl.rivm.screenit.mappers.config.ScreenitMapperConfig;
import nl.rivm.screenit.model.colon.RoosterItemListViewWrapper;
import nl.rivm.screenit.model.colon.dto.ColonAfspraakSlotDto;
import nl.rivm.screenit.util.DateUtil;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;

@Mapper(config = ScreenitMapperConfig.class)
public interface ColonAfspraakSlotMapper
{
	@Named("dateToLocalDateTime")
	default LocalDateTime conversionDatum(Date datum)
	{
		return DateUtil.toLocalDateTime(datum);
	}

	@Mapping(source = "roosterItemId", target = "id")
	@Mapping(source = "startTime", target = "startTime", qualifiedByName = "dateToLocalDateTime")
	@Mapping(source = "endTime", target = "endTime", qualifiedByName = "dateToLocalDateTime")
	@Mapping(source = "kamer", target = "kamer")
	@Mapping(source = "capaciteitMeeBepaald", target = "capaciteitMeeBepaald")
	@Mapping(source = "kamerId", target = "kamerId")
	@Mapping(target = "aantalBlokken", ignore = true)
	ColonAfspraakSlotDto roosterListItemViewWrapperToColonAfspraakDto(RoosterItemListViewWrapper wrapper);
}
