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

import nl.rivm.screenit.mamma.se.dto.LezingSeDto;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.enums.MammaZijde;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.util.NaamUtil;

public class VerslagLezingDtoMapper
{

	private DoorsnedeAfbeeldingenDtoMapper doorsnedeAfbeeldingenDtoMapper = new DoorsnedeAfbeeldingenDtoMapper();

	public LezingSeDto createVerslagLezingDto(MammaBeoordeling beoordeling, MammaBaseBeoordelingService beoordelingService)
	{
		LezingSeDto conclusie = new LezingSeDto();
		if (beoordeling == null)
		{
			return conclusie;
		}
		MammaLezing verslagLezing = beoordeling.getVerslagLezing();
		conclusie.setBiradsLinks(beoordelingService.getResultaatVoorZijde(beoordeling, MammaZijde.LINKER_BORST) != null
			? beoordelingService.getResultaatVoorZijde(beoordeling, MammaZijde.LINKER_BORST).getNaam()
			: null);
		conclusie.setBiradsRechts(beoordelingService.getResultaatVoorZijde(beoordeling, MammaZijde.RECHTER_BORST) != null
			? beoordelingService.getResultaatVoorZijde(beoordeling, MammaZijde.RECHTER_BORST).getNaam()
			: null);

		if (verslagLezing != null && verslagLezing.getLaesies() != null)
		{
			conclusie.setLezingAanzichten(doorsnedeAfbeeldingenDtoMapper.createDoorsnedeAfbeeldingenDtoFromLaesies(verslagLezing.getLaesies()));
			conclusie.setLezingType(verslagLezing.getLezingType().getNaam());
			conclusie.setRadioloogNaam(NaamUtil.getNaamGebruiker(verslagLezing.getBeoordelaar().getMedewerker()));
		}
		return conclusie;
	}
}
