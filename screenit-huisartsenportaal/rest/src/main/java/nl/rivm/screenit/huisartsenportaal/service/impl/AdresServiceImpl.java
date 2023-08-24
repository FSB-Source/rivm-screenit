package nl.rivm.screenit.huisartsenportaal.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.huisartsenportaal.dto.AdresDto;
import nl.rivm.screenit.huisartsenportaal.model.Adres;
import nl.rivm.screenit.huisartsenportaal.repository.AdresRepository;
import nl.rivm.screenit.huisartsenportaal.service.AdresService;
import nl.rivm.screenit.huisartsenportaal.service.WoonplaatsService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;

@Slf4j
@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class AdresServiceImpl implements AdresService
{

	@Autowired
	private WoonplaatsService woonplaatsService;

	@Autowired
	private AdresRepository adresRepository;

	private static final ObjectMapper objectMapper = new ObjectMapper();

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public Adres updateAndGetAdres(AdresDto adresDto)
	{
		Adres adres = null;
		if (adresDto.getHuisartsportaalId() != null)
		{
			adres = adresRepository.findByHuisartsportaalId(adresDto.getHuisartsportaalId());
		}
		if (adres == null && adresDto.getScreenitId() != null)
		{
			adres = adresRepository.findByScreenitId(adresDto.getScreenitId());
		}
		if (adres == null)
		{
			adres = new Adres();
		}
		if (adres.getScreenitId() == null)
		{
			adres.setScreenitId(adresDto.getScreenitId());
		}
		if (adresDto.getPostcode() == null)
		{
			throw new IllegalStateException("Postcode onbekend");
		}
		adres.setStraat(adresDto.getStraat());
		adres.setHuisnummer(adresDto.getHuisnummer());
		adres.setHuisnummertoevoeging(adresDto.getHuisnummertoevoeging());
		adres.setPostcode(adresDto.getPostcode().toUpperCase());
		var woonplaats = woonplaatsService.getWoonplaats(adresDto.getWoonplaats());
		if (woonplaats == null)
		{
			try
			{
				ObjectWriter writer = objectMapper.writer();
				LOG.error("Geen woonplaats gevonden met {}", writer.writeValueAsString(adresDto.getWoonplaats()));
			}
			catch (JsonProcessingException e)
			{
				LOG.error("Geen woonplaats gevonden", e);
			}
			throw new IllegalStateException("Woonplaats onbekend");
		}
		adres.setWoonplaats(woonplaats);
		adresRepository.save(adres);
		return adres;
	}
}
