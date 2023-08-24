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

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

import nl.rivm.screenit.mamma.se.proxy.services.LogischeSessieService;
import nl.rivm.screenit.mamma.se.proxy.services.ProxyService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/passanten")
public class PassantProxyController
{
	@Autowired
	private ProxyService proxyService;

	@Autowired
	private LogischeSessieService logischeSessieService;

	@RequestMapping(method = RequestMethod.GET)
	public ResponseEntity<String> readPassant(@RequestParam("bsn") String bsn,
		@RequestParam("geboortedatum") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate geboortedatum, @RequestHeader("accountId") String accountId, @RequestHeader("YubikeyIdentificatie") String yubikeyIdentificatie)
	{
		if(!logischeSessieService.geldigeYubikey(yubikeyIdentificatie))
		{
			return ResponseEntity.status(HttpStatus.FORBIDDEN).build();
		}

		RequestEntity.BodyBuilder requestBuilder = proxyService.getProxyRequestEntityAccount("/passanten?bsn=" + bsn + "&geboortedatum=" + geboortedatum.format(DateTimeFormatter.ISO_DATE), HttpMethod.GET, accountId);
		return proxyService.sendUncheckedProxyRequest(requestBuilder.build(), String.class);
	}
}
