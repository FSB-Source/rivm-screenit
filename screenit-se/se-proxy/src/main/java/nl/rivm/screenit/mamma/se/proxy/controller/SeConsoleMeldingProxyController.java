package nl.rivm.screenit.mamma.se.proxy.controller;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.mamma.se.proxy.model.ConsoleMeldingDto;

import org.apache.commons.lang3.StringUtils;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import jakarta.servlet.http.HttpServletRequest;

@RestController
@Slf4j
@RequestMapping("/api/console-melding")
public class SeConsoleMeldingProxyController
{
	@PostMapping("/{medewerkerCode}")
	public ResponseEntity<String> verwerkConsoleBericht(@RequestBody ConsoleMeldingDto meldingDto, @PathVariable String medewerkerCode, HttpServletRequest request)
	{
		var logBericht = maakLogBericht(request, medewerkerCode, meldingDto.getMelding(), meldingDto.getStack());

		switch (meldingDto.getLevel())
		{
		case ERROR:
			LOG.error(logBericht);
			break;
		case WARN:
			LOG.warn(logBericht);
			break;
		case LOG:
		case TRACE:
		default:
			LOG.info(logBericht);
			break;
		}
		return ResponseEntity.ok().build();
	}

	private String maakLogBericht(HttpServletRequest request, String medewerkerCode, String melding, String stack)
	{
		return String.format("[IP: %s, medewerkerCode: %s]: %s, %s", request.getRemoteAddr(), medewerkerCode, URLDecoder.decode(melding, StandardCharsets.UTF_8),
			StringUtils.isNotBlank(stack) ? "Stacktrace: " + stack : "");
	}
}
