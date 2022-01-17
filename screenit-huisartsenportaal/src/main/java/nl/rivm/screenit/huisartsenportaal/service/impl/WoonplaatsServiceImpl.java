
package nl.rivm.screenit.huisartsenportaal.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2016 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.huisartsenportaal.dto.WoonplaatsDto;
import nl.rivm.screenit.huisartsenportaal.model.Woonplaats;
import nl.rivm.screenit.huisartsenportaal.repository.WoonplaatsRepository;
import nl.rivm.screenit.huisartsenportaal.service.WoonplaatsService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class WoonplaatsServiceImpl implements WoonplaatsService
{
	@Autowired
	private WoonplaatsRepository woonplaatsRepository;

	@Override
	public void saveScreenITWoonplaats(WoonplaatsDto object)
	{
		Woonplaats woonplaats = woonplaatsRepository.findByScreenitId(object.getScreenitId());
		if (woonplaats == null)
		{
			woonplaats = new Woonplaats();
			woonplaats.setScreenitId(object.getScreenitId());
		}
		woonplaats.setNaam(object.getNaam());
		woonplaats.setCode(object.getCode());
		woonplaats.setGemeente(object.getGemeente());
		woonplaatsRepository.save(woonplaats);
	}

	@Override
	public List<WoonplaatsDto> getWoonplaatsen(String value)
	{
		Iterable<Woonplaats> woonplaatsen = woonplaatsRepository.find(value + "%");
		List<WoonplaatsDto> woonplaatsDtos = new ArrayList<WoonplaatsDto>();
		woonplaatsen.forEach(woonplaats -> {
			WoonplaatsDto woonplaatsDto = new WoonplaatsDto();
			woonplaatsDto.setGemeente(woonplaats.getGemeente());
			woonplaatsDto.setNaam(woonplaats.getNaam());
			woonplaatsDto.setHuisartsportaalId(woonplaats.getHuisartsportaalId());
			woonplaatsDtos.add(woonplaatsDto);
		});
		return woonplaatsDtos;
	}

	private String getDisplayNaam(Woonplaats woonplaats)
	{
		String display = woonplaats.getNaam();
		if (woonplaats.getGemeente() != null)
		{
			display += " (" + woonplaats.getGemeente() + ")";
		}
		return display;

	}

	@Override
	public Woonplaats setWoonplaats(WoonplaatsDto woonplaatsDto)
	{
		Woonplaats woonplaats = null;
		if (woonplaatsDto.getScreenitId() != null)
		{
			woonplaats = woonplaatsRepository.findByScreenitId(woonplaatsDto.getScreenitId());
		}
		if (woonplaats == null && woonplaatsDto.getHuisartsportaalId() != null)
		{
			woonplaats = woonplaatsRepository.findByHuisartsportaalId(woonplaatsDto.getHuisartsportaalId());
		}
		return woonplaats;
	}
}
