package nl.rivm.screenit.mamma.se.service.dtomapper;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import nl.rivm.screenit.mamma.se.dto.onderzoek.SignalerenSeDto;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaSignaleren;

public class SignalerenDtoMapper
{
	private DoorsnedeAfbeeldingenDtoMapper doorsnedeAfbeeldingenDtoMapper = new DoorsnedeAfbeeldingenDtoMapper();

	public SignalerenSeDto createSignalerenDto(MammaOnderzoek onderzoek)
	{
		return onderzoek != null && onderzoek.getSignaleren() != null ? createSignalerenDtoFromMammaSignaleren(onderzoek.getSignaleren()) : null;
	}

	private SignalerenSeDto createSignalerenDtoFromMammaSignaleren(MammaSignaleren signaleren)
	{
		SignalerenSeDto signalerenDto = new SignalerenSeDto();
		signalerenDto.setId(signaleren.getId());
		signalerenDto.setHeeftAfwijkingen(signaleren.isHeeftAfwijkingen());
		signalerenDto.setDoorsnedeAfbeeldingen(doorsnedeAfbeeldingenDtoMapper.createDoorsnedeAfbeeldingenDtoFromAanzichten(signaleren.getRechtsVerticaleDoorsnede(),
			signaleren.getLinksVerticaleDoorsnede(), signaleren.getRechtsHorizontaleDoorsnede(), signaleren.getLinksHorizontaleDoorsnede()));
		return signalerenDto;
	}

}
