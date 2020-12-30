
package nl.rivm.screenit.batch.jms.listener;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import javax.jms.JMSException;
import javax.jms.Session;

import nl.rivm.screenit.batch.service.VerwerkCdaBerichtService;

import org.apache.activemq.command.ActiveMQObjectMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jms.listener.SessionAwareMessageListener;

public class JMSVerwerkCdaBerichtListener implements SessionAwareMessageListener<ActiveMQObjectMessage>
{

	private static final Logger LOG = LoggerFactory.getLogger(JMSVerwerkCdaBerichtListener.class);

	@Autowired
	private VerwerkCdaBerichtService verwerkCdaBerichtService;

	@Override
	public void onMessage(ActiveMQObjectMessage message, Session session) throws JMSException
	{
		boolean rollback = false;
		Long berichtID = null;
		try
		{
			LOG.trace("ActiveMQ bericht " + message.getJMSMessageID() + " is ontvangen");

			Object object = message.getObject();

			if (object instanceof Long)
			{

				berichtID = (Long) object;

				if (LOG.isTraceEnabled())
				{
					LOG.trace("ActiveMQ bericht " + message.getJMSMessageID() + " met berichtID: " + berichtID);
				}
				verwerkCdaBerichtService.verwerkBericht(berichtID);
			}
		}
		catch (Exception e)
		{
			verwerkCdaBerichtService.verwerkError(berichtID, e);
			LOG.error("Exception bij verwerken van activeMQ bericht " + message.getJMSMessageID(), e);
			rollback = true;
		}

		if (rollback)
		{
			LOG.trace("ActiveMQ bericht " + message.getJMSMessageID() + " zal worden gerollbacked.");
			session.rollback();
		}
		else if (LOG.isTraceEnabled())
		{
			LOG.trace("ActiveMQ bericht " + message.getJMSMessageID() + " is succesvol verwerkt.");
		}

	}
}
