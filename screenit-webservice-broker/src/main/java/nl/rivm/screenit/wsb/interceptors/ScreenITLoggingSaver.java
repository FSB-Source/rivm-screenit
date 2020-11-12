package nl.rivm.screenit.wsb.interceptors;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.service.TechnischeBerichtenLoggingSaverService;
import nl.topicuszorg.spring.injection.SpringBeanProvider;

import org.apache.commons.lang3.StringUtils;
import org.apache.cxf.ext.logging.event.EventType;
import org.apache.cxf.ext.logging.event.LogEvent;
import org.apache.cxf.ext.logging.event.LogEventSender;

public class ScreenITLoggingSaver implements LogEventSender
{
	private TechnischeBerichtenLoggingSaverService technischeBerichtenLoggingSaverService;

	public ScreenITLoggingSaver()
	{
		technischeBerichtenLoggingSaverService = SpringBeanProvider.getInstance().getBean(TechnischeBerichtenLoggingSaverService.class);
	}

	@Override
	public void send(LogEvent event)
	{
		String type = "HL7V3_" + event.getType().toString();
		Long exchangeId = Long.valueOf(event.getExchangeId());
		String message = formatEvent(event);
		if (event.getType() == EventType.REQ_IN)
		{
			technischeBerichtenLoggingSaverService.logRequest(type, exchangeId, event.getOperationName(), message);
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
		write(b, "Payload", event.getPayload());
		return b.toString();
	}

	private void write(StringBuilder b, String key, String value)
	{
		if (StringUtils.isNotBlank(value))
		{
			b.append(key).append(": ").append(value).append("\n");
		}
	}

	public Long createExchangeId()
	{
		return technischeBerichtenLoggingSaverService.createExchangeId();
	}

}
