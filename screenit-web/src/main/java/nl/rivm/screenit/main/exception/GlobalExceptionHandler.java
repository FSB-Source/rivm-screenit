package nl.rivm.screenit.main.exception;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.IOException;
import java.util.stream.Collectors;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;

import org.apache.wicket.Application;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

import com.fasterxml.jackson.databind.ObjectMapper;

@Slf4j
@AllArgsConstructor
@Order(Ordered.HIGHEST_PRECEDENCE)
@ControllerAdvice
public class GlobalExceptionHandler
{
	private final ObjectMapper objectMapper = new ObjectMapper();

	@ExceptionHandler(ValidatieException.class)
	public ResponseEntity<String> handleValidatieException(ValidatieException ex)
	{
		var message = ex.getFormattedMessage(GlobalExceptionHandler.this::getString);
		LOG.error(message);
		var node = objectMapper.createObjectNode();
		node.put("message", message);
		return ResponseEntity.status(HttpStatus.UNPROCESSABLE_ENTITY).body(node.toString());
	}

	@ExceptionHandler(BeperkingException.class)
	public ResponseEntity<String> handleBeperkingException(BeperkingException ex) throws IOException
	{
		var messages = ex.getExceptions().stream().map(exception -> exception.getFormattedMessage(GlobalExceptionHandler.this::getString)).collect(
			Collectors.toList());
		messages.forEach(LOG::error);
		var messagesArrayNode = objectMapper.valueToTree(messages);
		var node = objectMapper.createObjectNode();
		node.set("messages", messagesArrayNode);
		node.put("beperkingType", ex.getBeperkingType().toString());
		return ResponseEntity.status(HttpStatus.UNPROCESSABLE_ENTITY).body(objectMapper.writeValueAsString(node));
	}

	@ExceptionHandler(BulkAanmakenException.class)
	public ResponseEntity<String> handleBulkAanmakenException(BulkAanmakenException ex)
	{
		return ResponseEntity.status(HttpStatus.UNPROCESSABLE_ENTITY).body(ex.toJson());
	}

	@ExceptionHandler(OpslaanVerwijderenTijdBlokException.class)
	public ResponseEntity<String> handleTijdBlokOverlapException(OpslaanVerwijderenTijdBlokException ex)
	{
		var message = getString(ex.getMessage());
		LOG.error(message);
		var node = objectMapper.createObjectNode();
		node.put("message", message);
		node.put("additionalInfo", ex.getAdditionalMessageInfo());
		return ResponseEntity.status(HttpStatus.UNPROCESSABLE_ENTITY).body(node.toString());
	}

	@ExceptionHandler(BulkVerwijderenException.class)
	public ResponseEntity<String> handleBulkVerwijderenException(BulkVerwijderenException ex)
	{
		LOG.error(ex.getMessage());
		return ResponseEntity.status(HttpStatus.UNPROCESSABLE_ENTITY).body(ex.toJson());
	}

	@ExceptionHandler(IllegalStateException.class)
	protected ResponseEntity<Object> handleIllegalStateException(IllegalStateException ex)
	{
		var message = getString(ex.getMessage());
		LOG.error(message);
		var node = objectMapper.createObjectNode();
		node.put("message", message);
		return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(node.toString());
	}

	@ExceptionHandler(Exception.class)
	protected ResponseEntity<Object> handleAllExceptions(Exception ex)
	{
		var message = getString("error.algemene.controller.error");
		LOG.error(message, ex);
		return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
	}

	private String getString(String key)
	{
		return Application.get().getResourceSettings().getLocalizer().getString(key, null);
	}
}
