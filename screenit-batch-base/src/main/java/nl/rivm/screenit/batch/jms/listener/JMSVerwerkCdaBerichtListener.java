package nl.rivm.screenit.batch.jms.listener;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.util.List;

import javax.jms.JMSException;
import javax.jms.Session;

import nl.rivm.screenit.batch.service.VerwerkCdaBerichtService;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;

import org.apache.activemq.command.ActiveMQMessage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jms.listener.SessionAwareMessageListener;

public class JMSVerwerkCdaBerichtListener implements SessionAwareMessageListener<ActiveMQMessage>
{

	@Autowired
	private VerwerkCdaBerichtService verwerkCdaBerichtService;

	private Bevolkingsonderzoek bvo;

	@Override
	public void onMessage(ActiveMQMessage message, Session session) throws JMSException
	{
		List<Long> berichten = verwerkCdaBerichtService.getAlleNietVerwerkteCdaBerichten(bvo);
		for (Long berichtId : berichten)
		{
			try
			{
				verwerkCdaBerichtService.verwerkBericht(berichtId);
			}
			catch (Exception e)
			{
				verwerkCdaBerichtService.verwerkError(berichtId, e);
			}
		}
		session.commit();
	}

	public void setBvo(Bevolkingsonderzoek bvo)
	{
		this.bvo = bvo;
	}
}
