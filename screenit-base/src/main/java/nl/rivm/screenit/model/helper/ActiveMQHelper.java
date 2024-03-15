package nl.rivm.screenit.model.helper;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.Serializable;

import javax.jms.JMSException;

import lombok.extern.slf4j.Slf4j;

import org.apache.activemq.command.ActiveMQObjectMessage;
import org.apache.activemq.command.ActiveMQTextMessage;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.fasterxml.jackson.databind.SerializationFeature;

@Slf4j
public class ActiveMQHelper
{
	private static final ObjectMapper objectMapper = new ObjectMapper();

	private ActiveMQHelper()
	{
	}

	public static <T extends Serializable> ActiveMQObjectMessage getActiveMqObjectMessage(T object) throws JMSException
	{
		if (LOG.isTraceEnabled())
		{
			try
			{
				objectMapper.configure(SerializationFeature.FAIL_ON_EMPTY_BEANS, false);
				var writer = objectMapper.writer();
				LOG.trace(object.getClass().getSimpleName() + ": " + writer.writeValueAsString(object));
			}
			catch (JsonProcessingException e)
			{
				LOG.error("Fout bij maken JSON voor ActiveMQObjectMessage bericht", e);
			}
		}
		var objectMessage = new ActiveMQObjectMessage();
		objectMessage.setObject(object);
		return objectMessage;
	}

	public static <T extends Serializable> ActiveMQTextMessage getActiveMqTextMessage(T object) throws JMSException
	{
		ObjectWriter writer = objectMapper.writer();
		String text = null;
		try
		{
			var textMessage = new ActiveMQTextMessage();
			text = writer.writeValueAsString(object);
			textMessage.setText(text);
			return textMessage;
		}
		catch (JsonProcessingException e)
		{
			LOG.error("Fout bij maken JSON voor activemq text bericht", e);
			return null;
		}

	}
}
