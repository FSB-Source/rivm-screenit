package nl.rivm.screenit.main.exception;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
		var message = getString(ex.getMessage());
		LOG.error(message);
		var node = objectMapper.createObjectNode();
		node.put("message", message);
		return ResponseEntity.status(HttpStatus.UNPROCESSABLE_ENTITY).body(node.toString());
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
