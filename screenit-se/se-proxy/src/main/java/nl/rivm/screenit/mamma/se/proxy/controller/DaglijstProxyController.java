package nl.rivm.screenit.mamma.se.proxy.controller;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import javax.servlet.http.HttpSession;

import nl.rivm.screenit.mamma.se.proxy.model.DaglijstMetMutatiesDto;
import nl.rivm.screenit.mamma.se.proxy.services.LogischeSessieService;
import nl.rivm.screenit.mamma.se.proxy.services.SeDaglijstService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

@RestController
@RequestMapping("/api/daglijst")
public class DaglijstProxyController
{
	@Autowired
	private SeDaglijstService daglijstService;

	@Autowired
	private LogischeSessieService logischeSessieService;

	private final ObjectMapper objectMapper = new ObjectMapper();

	@RequestMapping(value = "/{datum}", method = RequestMethod.GET)
	public ResponseEntity<String> readDaglijst(@PathVariable @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate datum,
		@RequestHeader("YubikeyIdentificatie") String yubikeyIdentificatie) throws JsonProcessingException
	{
		if (!logischeSessieService.geldigeYubikey(yubikeyIdentificatie))
		{
			return ResponseEntity.status(HttpStatus.FORBIDDEN).build();
		}

		return ResponseEntity.ok(objectMapper.writeValueAsString(new DaglijstMetMutatiesDto(daglijstService.getDaglijst(datum), daglijstService.getDaglijstMutaties(datum))));
	}

	@RequestMapping(value = "/geforceerd/{datum}", method = RequestMethod.GET)
	public ResponseEntity<String> readDaglijstGeforceerd(@PathVariable @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate datum, HttpSession session,
		@RequestHeader("YubikeyIdentificatie") String yubikeyIdentificatie) throws JsonProcessingException
	{
		if (!logischeSessieService.geldigeYubikey(yubikeyIdentificatie))
		{
			return ResponseEntity.status(HttpStatus.FORBIDDEN).build();
		}

		String daglijst = daglijstService.getDaglijstGeforceerd(datum);
		if (daglijst != null)
		{
			return ResponseEntity
				.ok(objectMapper.writeValueAsString(new DaglijstMetMutatiesDto(daglijst, daglijstService.getDaglijstMutaties(datum))));
		}
		else
		{
			return ResponseEntity.status(HttpStatus.GATEWAY_TIMEOUT).build();
		}
	}
}
