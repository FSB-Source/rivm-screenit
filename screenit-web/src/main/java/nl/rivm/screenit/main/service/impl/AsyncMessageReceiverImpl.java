
package nl.rivm.screenit.main.service.impl;

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

import java.util.concurrent.Future;

import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.Session;

import nl.rivm.screenit.main.service.AsyncMessageReceiver;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jms.core.JmsTemplate;
import org.springframework.jms.core.SessionCallback;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.AsyncResult;
import org.springframework.stereotype.Service;

@Service
public class AsyncMessageReceiverImpl implements AsyncMessageReceiver
{

	private static final Logger LOGGER = LoggerFactory.getLogger(AsyncMessageReceiverImpl.class);

	@Autowired
	private JmsTemplate jmsTemplate;

	@Override
	@Async
	public Future<Message> receiveMessage(final Destination destination, final Long timeout)
	{
		LOGGER.trace("receiveMessage");
		return new AsyncResult<Message>(jmsTemplate.execute(new SessionCallback<Message>()
		{

			@Override
			public Message doInJms(Session session) throws JMSException
			{
				MessageConsumer messageConsumer = null;
				try
				{
					messageConsumer = session.createConsumer(destination);
					Message receive = messageConsumer.receive(timeout);
					LOGGER.trace("doInJms: messageConsumer.receive");
					return receive;
				}
				finally
				{
					if (messageConsumer != null)
					{
						messageConsumer.close();
					}
					LOGGER.trace("receiveMessage: finaly");
				}
			}
		}, true));
	}
}
