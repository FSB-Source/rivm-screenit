package nl.rivm.screenit.util.logging.cxf;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Base64;

import nl.rivm.screenit.service.TechnischeBerichtenLoggingSaverService;
import nl.rivm.screenit.util.StringUtil;

import org.apache.commons.lang3.StringUtils;
import org.apache.cxf.ext.logging.event.EventType;
import org.apache.cxf.ext.logging.event.LogEvent;
import org.apache.cxf.ext.logging.event.LogEventSender;
import org.apache.cxf.message.Exchange;
import org.apache.cxf.message.Message;
import org.springframework.beans.factory.annotation.Autowired;

public class ScreenITLoggingSaver implements LogEventSender
{

	@Autowired
	private TechnischeBerichtenLoggingSaverService technischeBerichtenLoggingSaverService;

	@Override
	public void send(LogEvent event)
	{
		String type = "HL7V3_" + event.getType().toString();
		Long exchangeId = Long.valueOf(event.getExchangeId());
		String message = formatEvent(event);
		if (event.getType() == EventType.REQ_IN || event.getType() == EventType.REQ_OUT)
		{
			String operationName = event.getOperationName();
			if (StringUtils.isBlank(operationName) && event.getPortName() != null)
			{
				operationName = event.getPortName().toString();
			}
			if (StringUtils.isBlank(operationName) && event.getPortTypeName() != null)
			{
				operationName = event.getPortTypeName().toString();
			}
			if (StringUtils.isBlank(operationName))
			{
				operationName = "Onbekend " + type;
			}
			technischeBerichtenLoggingSaverService.logRequest(type, exchangeId, operationName, message);
		}
		else
		{
			technischeBerichtenLoggingSaverService.logResponse(type, exchangeId, message);
		}
	}

	private String formatEvent(LogEvent event)
	{
		StringBuilder b = new StringBuilder();
		write(b, "Address", event.getAddress());
		write(b, "HttpMethod", event.getHttpMethod());
		write(b, "Content-Type", event.getContentType());
		write(b, "ResponseCode", event.getResponseCode());
		write(b, "Principal", event.getPrincipal());
		write(b, "Encoding", event.getEncoding());
		write(b, "MessageId", event.getMessageId());

		if (event.getFullContentFile() != null)
		{
			write(b, "FullContentFile", event.getFullContentFile().getAbsolutePath());
		}
		if (event.getHeaders() != null)
		{
			write(b, "Headers", event.getHeaders().toString());
		}
		String payload = event.getPayload();
		if (StringUtil.containsControlCharacter(payload))
		{
			write(b, "Payload (Base64)", Base64.getEncoder().encodeToString(payload.getBytes()));
		}
		else if (payload != null)
		{
			write(b, "Payload", payload);
		}
		return b.toString();
	}

	private void write(StringBuilder b, String key, String value)
	{
		if (StringUtils.isNotBlank(value))
		{
			b.append(key).append(": ").append(value).append("\n");
		}
	}

	public void createExchangeId(Message message)
	{
		Exchange exchange = message.getExchange();
		String exchangeId = (String) exchange.get(LogEvent.KEY_EXCHANGE_ID);
		if (exchangeId == null)
		{
			exchangeId = Long.toString(technischeBerichtenLoggingSaverService.createExchangeId());
			exchange.put(LogEvent.KEY_EXCHANGE_ID, exchangeId);
		}
	}

}
