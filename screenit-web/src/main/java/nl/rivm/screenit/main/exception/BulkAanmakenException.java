package nl.rivm.screenit.main.exception;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.model.colon.enums.ColonRoosterBeperking;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.rivm.screenit.util.DateUtil;

import org.apache.wicket.Application;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

@Getter
@Setter
@NoArgsConstructor
public class BulkAanmakenException extends Exception
{
	private final List<BulkAanmakenInnerException> innerExceptions = new ArrayList<>();

	private final ObjectMapper objectMapper = new ObjectMapper();

	public void addException(RoosterItem afspraakslot, Exception exception)
	{
		var innerException = new BulkAanmakenInnerException(exception, afspraakslot);
		if (!innerExceptions.contains(innerException))
		{
			innerExceptions.add(innerException);
		}
	}

	public boolean isEmpty()
	{
		return innerExceptions.isEmpty();
	}

	public String toJson()
	{
		var exceptions = new ArrayList<ObjectNode>();
		String message;
		ColonRoosterBeperking type;
		ObjectNode node;
		RoosterItem afspraakslot;
		for (var innerException : innerExceptions)
		{
			if (innerException.getException() instanceof BeperkingException)
			{
				for (var validatieException : ((BeperkingException) innerException.getException()).getExceptions())
				{
					message = getMessage(validatieException);
					type = getExceptionType(innerException.getException());
					afspraakslot = innerException.getAfspraakslot();
					node = createNode(type, message, afspraakslot);
					exceptions.add(node);
				}
			}
			else
			{
				message = getMessage(innerException.getException());
				type = getExceptionType(innerException.getException());
				afspraakslot = innerException.getAfspraakslot();
				node = createNode(type, message, afspraakslot);
				exceptions.add(node);
			}
		}
		var messagesArrayNode = objectMapper.valueToTree(exceptions);
		var messagesNode = objectMapper.createObjectNode();
		messagesNode.set("exceptions", messagesArrayNode);
		messagesNode.put("type", "bulk");
		try
		{
			return objectMapper.writeValueAsString(messagesNode);
		}
		catch (JsonProcessingException e)
		{
			throw new RuntimeException(e);
		}
	}

	private ObjectNode createNode(ColonRoosterBeperking type, String exception, RoosterItem afspraakslot)
	{
		var node = objectMapper.createObjectNode();
		node.put("type", type.name());
		node.put("exception", exception);
		node.set("afspraakslot", afspraakslotToJson(afspraakslot));
		return node;
	}

	private ObjectNode afspraakslotToJson(RoosterItem afspraakslot)
	{
		var node = objectMapper.createObjectNode();
		node.put("id", afspraakslot.getId());
		node.put("startTime", DateUtil.formatTime(afspraakslot.getStartTime()));
		node.put("endTime", DateUtil.formatTime(afspraakslot.getEndTime()));
		node.put("datum", DateUtil.formatShortDate(afspraakslot.getStartTime()));
		return node;
	}

	private ColonRoosterBeperking getExceptionType(Exception exception)
	{
		var hardeExcepties = List.of("OpslaanVerwijderenTijdBlokException", "ValidatieException", "HeeftAfsprakenException");
		if (hardeExcepties.contains(exception.getClass().getSimpleName()))
		{
			return ColonRoosterBeperking.HARD;
		}

		if (exception instanceof BeperkingException)
		{
			return ((BeperkingException) exception).getBeperkingType();
		}

		return ColonRoosterBeperking.ZACHT;
	}

	private String getMessage(Exception exception)
	{
		String message;
		if (exception instanceof ValidatieException)
		{
			var validatieException = (ValidatieException) exception;
			message = getString(validatieException.getMessageKey());
			if (validatieException.getFormatArguments() != null)
			{
				message = String.format(message, validatieException.getFormatArguments()[0]);
			}
		}
		else if (exception instanceof OpslaanVerwijderenTijdBlokException)
		{
			var overlapException = (OpslaanVerwijderenTijdBlokException) exception;
			message = getString(overlapException.getMessage());
			if (overlapException.getAdditionalMessageInfo() != null)
			{
				message += " " + overlapException.getAdditionalMessageInfo();
			}
		}
		else
		{
			message = exception.getMessage();
		}
		return message;
	}

	private String getString(String key)
	{
		return Application.get().getResourceSettings().getLocalizer().getString(key, null);
	}
}
