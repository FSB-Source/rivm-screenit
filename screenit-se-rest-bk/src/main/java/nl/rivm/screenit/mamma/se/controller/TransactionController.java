package nl.rivm.screenit.mamma.se.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.mamma.se.dto.actions.ActionDto;
import nl.rivm.screenit.mamma.se.dto.actions.SETransactieType;
import nl.rivm.screenit.mamma.se.dto.actions.TransactionDto;
import nl.rivm.screenit.mamma.se.service.MammaScreeningsEenheidService;
import nl.rivm.screenit.mamma.se.service.SeTransactionService;
import nl.rivm.screenit.mamma.se.validation.DubbeleTijdValidator;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

@RestController
@RequestMapping("/api/transaction")
public class TransactionController extends AuthorizedController
{

	private static final Logger LOG = LoggerFactory.getLogger(TransactionController.class);

	private final ObjectMapper objectMapper = new ObjectMapper();

	@Autowired
	private SeTransactionService transactionService;

	@Autowired
	private MammaScreeningsEenheidService screeningsEenheidService;

	@Autowired
	private HibernateService hibernateService;

	private DubbeleTijdValidator dubbeleTijdValidator = new DubbeleTijdValidator();

	private final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@RequestMapping(method = RequestMethod.PUT)
	public ResponseEntity transaction(@RequestBody String transactionString, HttpServletRequest request,
		@RequestHeader(value = "TRANSACTIE_DATUMTIJD") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime transactieDatumTijd)
	{
		try
		{
			TransactionDto transactionDto = objectMapper.readValue(transactionString, TransactionDto.class);
			String seCode = request.getHeader("SE_CODE");
			MammaScreeningsEenheid se = screeningsEenheidService.getActieveScreeningsEenheidByCode(seCode);
			Long instellingGebruikerId = transactionDto.getInstellingGebruikerId();
			String uitnodigingsNr = transactionDto.getUitnodigingsNr();
			SETransactieType type = transactionDto.getType();
			List<ActionDto> acties = getActies(transactionString);
			LOG.info(
				String.format("[%s] SE[%s] Transactie ontvangen die is uitgevoerd op [%s], met type: %s, instellingGebruikerId: %s, clientid: %s, uitnodigingsNr: %s en acties: %s",
					currentDateSupplier.getLocalDateTime().format(formatter), seCode, transactieDatumTijd.format(formatter), type.toString(), instellingGebruikerId,
					transactionDto.getClientId(), uitnodigingsNr, acties.stream().map(ActionDto::getType).map(Objects::toString).collect(Collectors.joining(", "))));
			dubbeleTijdValidator.validate(acties);
			return transactionService.executeAsTransactionIfAuthorised(acties, transactionDto, transactieDatumTijd,
				hibernateService.get(InstellingGebruiker.class, instellingGebruikerId), se);
		}
		catch (Exception ex)
		{
			return createErrorResponse(ex);
		}
	}

	private List<ActionDto> getActies(String transactionString) throws IOException
	{
		ObjectNode transactionNode = objectMapper.readValue(transactionString, ObjectNode.class);
		ObjectNode[] actionNodes = objectMapper.treeToValue(transactionNode.get("actions"), ObjectNode[].class);
		List<ActionDto> acties = new ArrayList<>();
		for (ObjectNode actie : actionNodes)
		{
			ActionDto actionDto = objectMapper.readValue(actie.toString(), ActionDto.class);
			actionDto.setNodeText(actie.toString());
			acties.add(actionDto);
		}
		return acties;
	}
}
