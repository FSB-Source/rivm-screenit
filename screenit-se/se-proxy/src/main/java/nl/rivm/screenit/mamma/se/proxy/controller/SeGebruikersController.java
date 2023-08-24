package nl.rivm.screenit.mamma.se.proxy.controller;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Map;

import javax.servlet.http.HttpSession;

import nl.rivm.screenit.mamma.se.proxy.services.GebruikerStoreService;
import nl.rivm.screenit.mamma.se.proxy.services.LogischeSessieService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

@RestController
@RequestMapping("/api/seGebruiker")
public class SeGebruikersController
{
	private static final Logger LOG = LoggerFactory.getLogger(SeGebruikersController.class);

	private final ObjectMapper objectMapper = new ObjectMapper();

	@Autowired
	private GebruikerStoreService gebruikerStoreService;

	@Autowired
	private LogischeSessieService logischeSessieService;

	@RequestMapping(method = RequestMethod.GET)
	public ResponseEntity<String> getSeMedewerkers(HttpSession httpSession, @RequestHeader("YubikeyIdentificatie") String yubikeyIdentificatie)
	{
		if (!logischeSessieService.geldigeYubikey(yubikeyIdentificatie))
		{
			return ResponseEntity.status(HttpStatus.FORBIDDEN).build();
		}

		try
		{
			Map<Long, String> gebruikers = gebruikerStoreService.getSeGebruikers();
			return ResponseEntity.ok(objectMapper.writeValueAsString(gebruikers));
		}
		catch (JsonProcessingException e)
		{
			LOG.error(e.getMessage());
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
		}
	}
}
