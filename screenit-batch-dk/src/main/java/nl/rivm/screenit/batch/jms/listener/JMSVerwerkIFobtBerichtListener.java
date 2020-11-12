package nl.rivm.screenit.batch.jms.listener;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import nl.rivm.screenit.batch.service.IFobtHL7BerichtInlezenService;
import nl.rivm.screenit.model.colon.berichten.ColonIFobtUitslagBericht;
import org.apache.activemq.command.ActiveMQObjectMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jms.listener.SessionAwareMessageListener;

import javax.jms.JMSException;
import javax.jms.Session;
import java.util.List;

public class JMSVerwerkIFobtBerichtListener implements SessionAwareMessageListener<ActiveMQObjectMessage>
{
	
	private static final Logger LOG = LoggerFactory.getLogger(JMSVerwerkIFobtBerichtListener.class);

	@Autowired
	private IFobtHL7BerichtInlezenService iFobtHL7BerichtInlezenService;

	@Override
	public void onMessage(ActiveMQObjectMessage activeMQObjectMessage, Session session) throws JMSException
	{

		List<ColonIFobtUitslagBericht> berichten = iFobtHL7BerichtInlezenService.getAlleNietVerwerkteIFobtBerichten();

		for (ColonIFobtUitslagBericht bericht : berichten)
		{
			try
			{
				iFobtHL7BerichtInlezenService.verwerkOntvangenIFobtBericht(bericht);
			}
			catch (Exception e)
			{
				LOG.error("Er is een probleem opgetreden, met het verwerken van de hpv data", e);
				iFobtHL7BerichtInlezenService.logError(bericht, e.getMessage(), null);
			}
		}

	}
}
