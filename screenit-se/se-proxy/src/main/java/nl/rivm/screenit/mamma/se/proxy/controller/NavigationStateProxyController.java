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

import java.io.IOException;

import nl.rivm.screenit.mamma.se.proxy.model.LogischeSessie;
import nl.rivm.screenit.mamma.se.proxy.model.NavigatieDto;
import nl.rivm.screenit.mamma.se.proxy.services.LogischeSessieService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.fasterxml.jackson.databind.ObjectMapper;

@RestController
@RequestMapping("/api/navigationState")
public class NavigationStateProxyController
{
	private final ObjectMapper objectMapper = new ObjectMapper();

	private static final Logger LOG = LoggerFactory.getLogger(NavigationStateProxyController.class);

	@Autowired
	private LogischeSessieService logischeSessieService;

	@RequestMapping(method = RequestMethod.PUT, value = "/{yubikeyIdentificatie}")
	public ResponseEntity<String> wijzigNavigationState(@PathVariable String yubikeyIdentificatie, @RequestBody String navigationAction)
		throws IOException
	{
		navigationAction = navigationAction.replace("type", "navigatieType");
		NavigatieDto navigatie = objectMapper.readValue(navigationAction, NavigatieDto.class);

		LogischeSessie logischeSessie = logischeSessieService.getLogischeSessieMetIdentificatie(yubikeyIdentificatie);

		if (logischeSessie == null)
		{
			LOG.info("Logische sessie niet gevonden.");
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).build();
		}

		if (HttpStatus.FORBIDDEN.equals(logischeSessie.getLoginAntwoord().getStatusCode()))
		{
			logischeSessieService.verwijderLogischeSessie(logischeSessie);
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).build();
		}

		if (logischeSessieService.isVerlopen(logischeSessie))
		{
			logischeSessieService.verwijderLogischeSessie(logischeSessie);
			LOG.info("Logische sessie is verlopen.");
			return ResponseEntity.status(HttpStatus.REQUEST_TIMEOUT).build();
		}

		logischeSessieService.updateLogischeSessie(logischeSessie, navigatie);
		LOG.info("Navigatie: yubikey: {}, clientId: {}, {}-'{}'", logischeSessie.getYubikeyIdentificatie(), navigatie.getClientId(), navigatie.getNavigatieType(),
			navigatie.getSubPagina());
		return ResponseEntity.ok().build();
	}
}
