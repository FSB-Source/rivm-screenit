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

import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import nl.rivm.screenit.mamma.se.proxy.services.LogischeSessieService;
import nl.rivm.screenit.mamma.se.proxy.services.TransactionQueueService;
import nl.rivm.screenit.mamma.se.proxy.services.WebSocketProxyService;
import nl.rivm.screenit.mamma.se.proxy.util.TransactionParser;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import jakarta.servlet.http.HttpSession;

@RestController
@RequestMapping("/api/putTransactionToScreenItCentraal")
public class TransactionProxyController
{
	@Autowired
	private WebSocketProxyService webSocketProxyService;

	@Autowired
	private TransactionQueueService transactionQueueService;

	@Autowired
	private LogischeSessieService logischeSessieService;

	@RequestMapping(method = RequestMethod.PUT)
	public ResponseEntity<String> putTransactionToScreenItCentraal(@RequestBody String transactionJSON, HttpSession httpSession,
		@RequestHeader("YubikeyIdentificatie") String yubikeyIdentificatie) throws IOException
	{
		if (!logischeSessieService.geldigeYubikey(yubikeyIdentificatie))
		{
			return ResponseEntity.status(HttpStatus.FORBIDDEN).build();
		}
		if (transactionJSON.startsWith("["))
		{
			final JsonNode jsonNode = new ObjectMapper().readTree(transactionJSON);
			List<String> transacties = StreamSupport.stream(jsonNode.spliterator(), false).map(JsonNode::toString).collect(Collectors.toList());
			for (String transactie : transacties)
			{
				doTransaction(transactie);
			}
		}
		else
		{
			doTransaction(transactionJSON);
		}
		return ResponseEntity.ok().build();
	}

	private void doTransaction(String transactionJSON)
	{
		Long clientId = new TransactionParser(transactionJSON).getClientId();
		transactionQueueService.addTransactionToQueue(transactionJSON, clientId);
		webSocketProxyService.broadcast(transactionJSON);
	}

}
