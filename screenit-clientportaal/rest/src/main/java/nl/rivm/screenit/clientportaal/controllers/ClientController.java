package nl.rivm.screenit.clientportaal.controllers;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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

import nl.rivm.screenit.clientportaal.mappers.ClientMapper;
import nl.rivm.screenit.clientportaal.mappers.TijdelijkAdresMapper;
import nl.rivm.screenit.clientportaal.model.ClientDto;
import nl.rivm.screenit.clientportaal.model.TelefoonnummerDto;
import nl.rivm.screenit.clientportaal.model.TijdelijkAdresDto;
import nl.rivm.screenit.clientportaal.services.ClientGegevensService;
import nl.rivm.screenit.model.TijdelijkAdres;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("persoon")
public class ClientController extends AbstractController
{

	@Autowired
	private ClientMapper clientMapper;

	@Autowired
	private TijdelijkAdresMapper tijdelijkAdresMapper;

	@Autowired
	private ClientGegevensService clientGegevensService;

	@GetMapping
	public ResponseEntity<ClientDto> getCurrentUser(Authentication authentication)
	{
		return ResponseEntity.ok(clientMapper.clientToDto(getClient(authentication)));
	}

	@PutMapping("/telefoonnummer")
	public ResponseEntity<ClientDto> setTelefoonnummer(@RequestBody TelefoonnummerDto telefoonnummerDto, Authentication authentication)
	{
		clientGegevensService.setTelefoonnummer(telefoonnummerDto.getTelefoonnummer1(), telefoonnummerDto.getTelefoonnummer2(),
			getClient(authentication));
		return ResponseEntity.ok(clientMapper.clientToDto(getClient(authentication)));
	}

	@PutMapping("/tijdelijk-adres")
	public ResponseEntity<ClientDto> setTijdelijkAdres(@RequestBody TijdelijkAdresDto tijdelijkAdresDto, Authentication authentication)
	{
		TijdelijkAdres tijdelijkAdres = tijdelijkAdresMapper.dtoToTijdelijkAdres(tijdelijkAdresDto);
		clientGegevensService.setTijdelijkAdres(tijdelijkAdres, getClient(authentication));

		return ResponseEntity.ok(clientMapper.clientToDto(getClient(authentication)));
	}
}
