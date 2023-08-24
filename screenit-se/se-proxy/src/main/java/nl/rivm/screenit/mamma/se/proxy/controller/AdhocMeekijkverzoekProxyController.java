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

import javax.servlet.http.HttpSession;

import nl.rivm.screenit.mamma.se.proxy.services.LogischeSessieService;
import nl.rivm.screenit.mamma.se.proxy.services.ProxyService;
import nl.rivm.screenit.mamma.se.proxy.util.SafeStringUtil;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/adhocMeekijkverzoek")
public class AdhocMeekijkverzoekProxyController
{
	@Autowired
	private ProxyService proxyService;

	@Autowired
	private LogischeSessieService logischeSessieService;

	private static final Logger LOG = LoggerFactory.getLogger(AdhocMeekijkverzoekProxyController.class);

	@RequestMapping(value = "indienen/{afspraakId}", method = RequestMethod.POST)
	public ResponseEntity adhocControleIndienen(@RequestBody String reden, @PathVariable long afspraakId, HttpSession session,
		@RequestHeader("YubikeyIdentificatie") String yubikeyIdentificatie, @RequestHeader("accountId") String accountId)
	{
		var safeReden = SafeStringUtil.maakStringMetUserInputVeiligVoorLogging(reden);
		LOG.info("Meekijkverzoek LRCB aangevraagd voor afspraakId: " + afspraakId + " met reden: " + safeReden);
		if (!logischeSessieService.geldigeYubikey(yubikeyIdentificatie))
		{
			return ResponseEntity.status(HttpStatus.FORBIDDEN).build();
		}

		RequestEntity.BodyBuilder requestBuilder = proxyService.getProxyRequestEntityAccount("/adhocMeekijkverzoek/indienen/" + afspraakId, HttpMethod.POST, accountId);

		return proxyService.sendUncheckedProxyRequest(requestBuilder.body(reden), String.class);
	}

	@RequestMapping(value = "controleren/{afspraakId}", method = RequestMethod.POST)
	public ResponseEntity adhocControleControleren(@PathVariable long afspraakId, HttpSession session,
		@RequestHeader("YubikeyIdentificatie") String yubikeyIdentificatie, @RequestHeader("accountId") String accountId)
	{
		LOG.info("Gedrukt op meekijkverzoek LRCB pop-up knop voor afspraakId " + afspraakId);
		if (!logischeSessieService.geldigeYubikey(yubikeyIdentificatie))
		{
			return ResponseEntity.status(HttpStatus.FORBIDDEN).build();
		}

		RequestEntity.BodyBuilder requestBuilder = proxyService.getProxyRequestEntityAccount("/adhocMeekijkverzoek/controleren/" + afspraakId, HttpMethod.POST, accountId);

		return proxyService.sendUncheckedProxyRequest(requestBuilder.build(), String.class);
	}
}
