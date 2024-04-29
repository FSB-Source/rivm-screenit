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

import nl.rivm.screenit.exceptions.HeeftAfsprakenException;
import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.exceptions.TijdBlokOverlapException;
import nl.rivm.screenit.model.colon.enums.ColonRoosterBeperking;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment;

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

	public void addException(AbstractAppointment tijdslot, Exception exception)
	{
		var innerException = new BulkAanmakenInnerException(exception, tijdslot);
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
		AbstractAppointment tijdslot;
		for (var innerException : innerExceptions)
		{
			if (innerException.getException() instanceof BeperkingException)
			{
				for (var validatieException : ((BeperkingException) innerException.getException()).getExceptions())
				{
					type = getExceptionType(innerException.getException());
					tijdslot = innerException.getTijdslot();
					message = getMessage(validatieException, tijdslot);
					node = createNode(type, message, tijdslot);
					exceptions.add(node);
				}
			}
			else
			{
				type = getExceptionType(innerException.getException());
				tijdslot = innerException.getTijdslot();
				message = getMessage(innerException.getException(), tijdslot);
				node = createNode(type, message, tijdslot);
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

	private ObjectNode createNode(ColonRoosterBeperking type, String exception, AbstractAppointment tijdslot)
	{
		var node = objectMapper.createObjectNode();
		node.put("type", type.name());
		node.put("exception", exception);
		node.set("tijdslot", tijdslotToJson(tijdslot));
		return node;
	}

	private ObjectNode tijdslotToJson(AbstractAppointment tijdslot)
	{
		var node = objectMapper.createObjectNode();
		node.put("id", tijdslot.getId());
		node.put("startTime", DateUtil.formatTime(tijdslot.getStartTime()));
		node.put("endTime", DateUtil.formatTime(tijdslot.getEndTime()));
		node.put("datum", DateUtil.formatShortDate(tijdslot.getStartTime()));
		return node;
	}

	private ColonRoosterBeperking getExceptionType(Exception exception)
	{
		var hardeExcepties = List.of(OpslaanVerwijderenTijdBlokException.class, ValidatieException.class, HeeftAfsprakenException.class, TijdBlokOverlapException.class);
		if (hardeExcepties.contains(exception.getClass()))
		{
			return ColonRoosterBeperking.HARD;
		}

		if (exception instanceof BeperkingException)
		{
			return ((BeperkingException) exception).getBeperkingType();
		}

		return ColonRoosterBeperking.ZACHT;
	}

	private String getMessage(Exception exception, AbstractAppointment tijdslot)
	{
		String message;
		if (exception instanceof ValidatieException)
		{
			var validatieException = (ValidatieException) exception;
			message = validatieException.getFormattedMessage(BulkAanmakenException.this::getString);
		}
		else if (exception instanceof TijdBlokOverlapException)
		{
			message = getString(tijdslot instanceof RoosterItem ? "error.afspraakslot.heeft.overlap.bulk" : "error.blokkade.heeft.overlap.bulk");
		}
		else
		{
			message = getString(exception.getMessage());
		}
		return message;
	}

	private String getString(String key)
	{
		return Application.get().getResourceSettings().getLocalizer().getString(key, null, key);
	}

	public String getSamenvatting()
	{
		var samenvatting = "";

		var zachteBeperkingen = innerExceptions.stream().filter(e -> e.getException() instanceof BeperkingException)
			.map(e -> (BeperkingException) e.getException()).filter(e -> e.getBeperkingType() == ColonRoosterBeperking.ZACHT).count();
		var hardeBeperkingen = innerExceptions.stream().filter(e ->
		{
			if (e.getException() instanceof BeperkingException)
			{
				return ((BeperkingException) e.getException()).getBeperkingType() == ColonRoosterBeperking.HARD;
			}
			return true;
		}).count();

		if (zachteBeperkingen > 0)
		{
			samenvatting += String.format("%d aangemaakt op afwijkende dag/tijd", zachteBeperkingen);
		}
		if (hardeBeperkingen > 0)
		{
			if (!samenvatting.isEmpty())
			{
				samenvatting += ", ";
			}
			samenvatting += String.format("%d niet aangemaakt", hardeBeperkingen);
		}
		return samenvatting;
	}
}
