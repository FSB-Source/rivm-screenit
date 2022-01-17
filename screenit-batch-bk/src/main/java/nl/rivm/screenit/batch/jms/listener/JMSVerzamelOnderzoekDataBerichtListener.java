package nl.rivm.screenit.batch.jms.listener;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import javax.jms.Session;

import nl.rivm.screenit.batch.service.MammaVerzamelDownloadOnderzoekDataService;

import org.apache.activemq.command.ActiveMQTextMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jms.listener.SessionAwareMessageListener;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Transactional(propagation = Propagation.SUPPORTS)
public class JMSVerzamelOnderzoekDataBerichtListener implements SessionAwareMessageListener<ActiveMQTextMessage>
{
	private static final Logger LOG = LoggerFactory.getLogger(JMSVerzamelOnderzoekDataBerichtListener.class);

	@Autowired
	private MammaVerzamelDownloadOnderzoekDataService verzamelDownloadOnderzoekDataService;

	@Override
	public void onMessage(ActiveMQTextMessage message, Session session)
	{
		LOG.info("Verzamelen van onderzoekensdata is getriggerd.");
		try
		{
			verzamelDownloadOnderzoekDataService.getAlleVerzamelDownloadOnderzoekDataVerzoeken().forEach(verzamelDownloadOnderzoekDataService::verzamelOnderzoekData);
		}
		catch (Exception e)
		{
			LOG.error("Fout bij afwerken van downloadverzoeken", e);
		}
	}

}
