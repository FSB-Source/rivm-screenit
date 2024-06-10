package nl.rivm.screenit.mamma.se.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletRequest;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.mamma.se.dto.actions.ActionDto;
import nl.rivm.screenit.mamma.se.dto.actions.TransactionDto;
import nl.rivm.screenit.mamma.se.service.MammaScreeningsEenheidService;
import nl.rivm.screenit.mamma.se.service.SeTransactionService;
import nl.rivm.screenit.mamma.se.validation.DubbeleTijdValidator;
import nl.rivm.screenit.repository.algemeen.InstellingGebruikerRepository;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

@RestController
@RequestMapping("/api/transaction")
@Slf4j
public class TransactionController extends AuthorizedController
{
	@Autowired
	private InstellingGebruikerRepository instellingGebruikerRepository;

	private final ObjectMapper objectMapper = new ObjectMapper();

	@Autowired
	private SeTransactionService transactionService;

	@Autowired
	private MammaScreeningsEenheidService screeningsEenheidService;

	private final DubbeleTijdValidator dubbeleTijdValidator = new DubbeleTijdValidator();

	private final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

	@PutMapping
	public ResponseEntity transaction(@RequestBody String transactionString, HttpServletRequest request,
		@RequestHeader(value = "TRANSACTIE_DATUMTIJD") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime transactieDatumTijd)
	{
		try
		{
			var transactionDto = objectMapper.readValue(transactionString, TransactionDto.class);
			var seCode = request.getHeader("SE_CODE");
			var se = screeningsEenheidService.getActieveScreeningsEenheidByCode(seCode);
			var instellingGebruikerId = transactionDto.getInstellingGebruikerId();
			var uitnodigingsNr = transactionDto.getUitnodigingsNr();
			var type = transactionDto.getType();
			var acties = getActies(transactionString);
			LOG.info("SE[{}] Transactie ontvangen die is uitgevoerd op [{}], met type: {}, instellingGebruikerId: {}, clientid: {}, uitnodigingsNr: {} en acties: {}",
				seCode, transactieDatumTijd.format(formatter), type, instellingGebruikerId,
				transactionDto.getClientId(), uitnodigingsNr, acties.stream().map(ActionDto::getType).map(Objects::toString).collect(Collectors.joining(", ")));

			dubbeleTijdValidator.validate(acties);

			var instellingGebruiker = instellingGebruikerId != null ? instellingGebruikerRepository.findById(instellingGebruikerId).orElseThrow() : null;

			return transactionService.executeAsTransactionIfAuthorised(acties, transactionDto, transactieDatumTijd, instellingGebruiker, se);
		}
		catch (Exception ex)
		{
			return createErrorResponse(ex);
		}
	}

	private List<ActionDto> getActies(String transactionString) throws IOException
	{
		var transactionNode = objectMapper.readValue(transactionString, ObjectNode.class);
		var actionNodes = objectMapper.treeToValue(transactionNode.get("actions"), ObjectNode[].class);
		List<ActionDto> acties = new ArrayList<>();
		for (var actie : actionNodes)
		{
			var actionDto = objectMapper.readValue(actie.toString(), ActionDto.class);
			actionDto.setNodeText(actie.toString());
			acties.add(actionDto);
		}
		return acties;
	}
}
