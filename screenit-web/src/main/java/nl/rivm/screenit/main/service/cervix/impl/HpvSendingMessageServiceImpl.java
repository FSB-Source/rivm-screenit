package nl.rivm.screenit.main.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.main.service.cervix.HpvSendingMessageService;
import nl.rivm.screenit.model.berichten.ScreenITResponseV251MessageWrapper;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvBerichtWrapper;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import ca.uhn.hl7v2.DefaultHapiContext;
import ca.uhn.hl7v2.HL7Exception;
import ca.uhn.hl7v2.HapiContext;
import ca.uhn.hl7v2.app.Connection;
import ca.uhn.hl7v2.model.Message;
import ca.uhn.hl7v2.model.v251.message.OUL_R22;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class HpvSendingMessageServiceImpl implements HpvSendingMessageService
{

	private static final Logger LOG = LoggerFactory.getLogger(HpvSendingMessageServiceImpl.class);

	private HapiContext context = new DefaultHapiContext();

	@Autowired
	private Integer hpvPort;

	@Autowired
	private String hpvHost;

	@Override
	public Map<String, String> verstuurHpvBerichten(List<Message> messages)
	{
		Map<String, String> responseMap = new HashMap<String, String>();
		Connection conn = getConnection();
		try
		{
			if (conn != null)
			{
				for (Message message : messages)
				{
					CervixHpvBerichtWrapper wrapper = new CervixHpvBerichtWrapper((OUL_R22) message);
					String messageId = wrapper.getMessageId();
					Message response = conn.getInitiator().sendAndReceive(message);
					ScreenITResponseV251MessageWrapper responseMessageWrapper = new ScreenITResponseV251MessageWrapper(response);
					String mapWaarde = responseMessageWrapper.getAcknowledgmentCodeString() + responseMessageWrapper.getMelding();
					responseMap.put(messageId, mapWaarde);
				}
			}
		}
		catch (Exception e)
		{
			LOG.error("Er is mis gegaan met het versturen van de berichten", e);
			return null;
		}
		finally
		{
			closingConnection(conn);
		}
		return responseMap;
	}

	@Override
	public ScreenITResponseV251MessageWrapper verstuurHpvBericht(Message message)
	{
		ScreenITResponseV251MessageWrapper wrapper = null;
		Connection conn = getConnection();
		try
		{
			if (conn != null)
			{
				Message response = conn.getInitiator().sendAndReceive(message);
				wrapper = new ScreenITResponseV251MessageWrapper(response);
				LOG.info("Message verstuurd. Response was: " + wrapper.getAcknowledgmentCode());
			}
		}
		catch (Exception e)
		{
			LOG.error("Er is iets misgegaan met het versturen van een message", e);
		}
		finally
		{
			closingConnection(conn);
		}
		return wrapper;
	}

	private Connection getConnection()
	{
		Connection conn = null;
		try
		{
			LOG.info("Poging tot connectie, ");
			conn = context.newClient(hpvHost, hpvPort.intValue(), false);
		}
		catch (HL7Exception e)
		{
			LOG.error("Er is iets misgegaan met het opzetten van de verbinding", e);
		}
		return conn;
	}

	private boolean closingConnection(Connection conn)
	{
		if (conn != null && conn.isOpen())
		{
			conn.close();
			return conn.isOpen();
		}
		return false;
	}
}
