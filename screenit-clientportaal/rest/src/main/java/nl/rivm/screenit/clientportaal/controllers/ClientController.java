package nl.rivm.screenit.clientportaal.controllers;

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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.clientportaal.mappers.ClientMapper;
import nl.rivm.screenit.clientportaal.mappers.TijdelijkAdresMapper;
import nl.rivm.screenit.clientportaal.model.AanhefDto;
import nl.rivm.screenit.clientportaal.model.ClientDto;
import nl.rivm.screenit.clientportaal.model.EmailAdresDto;
import nl.rivm.screenit.clientportaal.model.TelefoonnummerDto;
import nl.rivm.screenit.clientportaal.model.TijdelijkAdresDto;
import nl.rivm.screenit.clientportaal.services.ClientGegevensService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.TijdelijkAdres;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("persoon")
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
@AllArgsConstructor
public class ClientController extends AbstractController
{
	private final HibernateService hibernateService;

	private final ClientMapper clientMapper;

	private final TijdelijkAdresMapper tijdelijkAdresMapper;

	private final ClientGegevensService clientGegevensService;

	@GetMapping
	public ResponseEntity<ClientDto> getCurrentUser(Authentication authentication)
	{
		return ResponseEntity.ok(clientMapper.clientToDto(getClient(authentication, hibernateService)));
	}

	@PutMapping("/telefoonnummer")
	public ResponseEntity<ClientDto> setTelefoonnummer(@RequestBody TelefoonnummerDto telefoonnummerDto, Authentication authentication)
	{
		Client client = getClient(authentication, hibernateService);
		clientGegevensService.setTelefoonnummer(telefoonnummerDto.getTelefoonnummer1(), telefoonnummerDto.getTelefoonnummer2(), client);
		return ResponseEntity.ok(clientMapper.clientToDto(client));
	}

	@PutMapping("email")
	public ResponseEntity<ClientDto> setEmail(@RequestBody EmailAdresDto emailAdresDto, Authentication authentication)
	{
		var client = getClient(authentication, hibernateService);
		clientGegevensService.setEmailadres(emailAdresDto.getEmailadres(), client);
		return ResponseEntity.ok(clientMapper.clientToDto(client));
	}

	@PutMapping("/tijdelijk-adres")
	public ResponseEntity<ClientDto> setTijdelijkAdres(@RequestBody TijdelijkAdresDto tijdelijkAdresDto, Authentication authentication)
	{
		Client client = getClient(authentication, hibernateService);
		TijdelijkAdres tijdelijkAdres = tijdelijkAdresMapper.dtoToTijdelijkAdres(tijdelijkAdresDto);
		clientGegevensService.setTijdelijkAdres(tijdelijkAdres, client);
		return ResponseEntity.ok(clientMapper.clientToDto(client));
	}

	@PutMapping("/aanhef")
	public ResponseEntity<ClientDto> setAanhef(@RequestBody AanhefDto aanhefDto, Authentication authentication)
	{
		Client client = getClient(authentication, hibernateService);
		clientGegevensService.setAanhef(aanhefDto.getAanhef(), client);
		return ResponseEntity.ok(clientMapper.clientToDto(client));
	}
}
