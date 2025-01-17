package nl.rivm.screenit.mappers.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.time.LocalDateTime;
import java.util.Date;

import nl.rivm.screenit.mappers.config.ScreenitMapperConfig;
import nl.rivm.screenit.model.colon.ColonAfspraakslotListViewWrapper;
import nl.rivm.screenit.model.colon.dto.ColonAfspraakslotDto;
import nl.rivm.screenit.util.DateUtil;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;

@Mapper(config = ScreenitMapperConfig.class)
public interface ColonAfspraakslotMapper
{
	@Named("dateToLocalDateTime")
	default LocalDateTime conversionDatum(Date datum)
	{
		return DateUtil.toLocalDateTime(datum);
	}

	@Mapping(source = "afspraakslotId", target = "id")
	@Mapping(source = "startDatum", target = "vanaf", qualifiedByName = "dateToLocalDateTime")
	@Mapping(source = "eindDatum", target = "tot", qualifiedByName = "dateToLocalDateTime")
	@Mapping(source = "kamer", target = "kamer")
	@Mapping(source = "capaciteitMeeBepaald", target = "capaciteitMeeBepaald")
	@Mapping(source = "kamerId", target = "kamerId")
	@Mapping(target = "aantalBlokken", ignore = true)
	@Mapping(target = "alleenValidatie", ignore = true)
	@Mapping(target = "herhaling", ignore = true)
	ColonAfspraakslotDto roosterListItemViewWrapperToColonAfspraakslotDto(ColonAfspraakslotListViewWrapper wrapper);
}
