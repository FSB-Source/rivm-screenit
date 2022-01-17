package nl.rivm.screenit.mamma.se.service.dtomapper;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import nl.rivm.screenit.mamma.se.dto.MammaHuisartsDto;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.enums.HuisartsGeslacht;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.NaamUtil;

public class HuisartsDtoMapper
{
	private HuisartsDtoMapper()
	{
	}

	public static MammaHuisartsDto createMammaHuisarsDto(EnovationHuisarts enovationHuisarts)
	{
		if (enovationHuisarts == null)
		{
			return null;
		}
		MammaHuisartsDto huisartsDto = new MammaHuisartsDto();
		huisartsDto.setId(enovationHuisarts.getId());
		huisartsDto.setNaamHuisarts(NaamUtil.getNaamHuisarts(enovationHuisarts));
		huisartsDto.setAchternaam(enovationHuisarts.getAchternaam());
		huisartsDto.setPraktijknaam(enovationHuisarts.getPraktijknaam());
		huisartsDto.setWeergaveNaam(enovationHuisarts.getWeergavenaam());
		if (enovationHuisarts.getAdres() != null)
		{
			huisartsDto.setStraatnaam(enovationHuisarts.getAdres().getStraat());
			huisartsDto.setHuisnummerVolledig(AdresUtil.getHuisnummerVolledig(enovationHuisarts.getAdres()));
			huisartsDto.setPostcode(enovationHuisarts.getAdres().getPostcode());
			huisartsDto.setPlaats(enovationHuisarts.getAdres().getPlaats());
		}
		huisartsDto.setType(HuisartsGeslacht.ORGANISATIE.equals(enovationHuisarts.getGeslacht()) ? "Huisartsenpraktijk" : "Huisarts");
		return huisartsDto;
	}
}
