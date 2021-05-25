package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.IOException;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.service.HL7TestMessageService;
import nl.rivm.screenit.model.berichten.ScreenITResponseV24MessageWrapper;
import nl.rivm.screenit.model.berichten.ScreenITResponseV251MessageWrapper;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import ca.uhn.hl7v2.DefaultHapiContext;
import ca.uhn.hl7v2.HL7Exception;
import ca.uhn.hl7v2.HapiContext;
import ca.uhn.hl7v2.app.Connection;
import ca.uhn.hl7v2.llp.LLPException;
import ca.uhn.hl7v2.model.Message;
import ca.uhn.hl7v2.parser.Parser;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class HL7TestMessageServiceImpl implements HL7TestMessageService
{

	private static final Logger LOG = LoggerFactory.getLogger(HL7TestMessageServiceImpl.class);

	private HapiContext context = new DefaultHapiContext();

	@Qualifier("hl7IfobtPort")
	@Autowired(required = false)
	private Integer hl7IfobtPort;

	@Qualifier("hl7IfobtHost")
	@Autowired(required = false)
	private String hl7wsbHost;

	private Integer hl7IMSOrmPort;

	private Integer hl7IMSAdtPort;

	private String hl7IMSHost;

	@Qualifier("hl7ImsPort")
	@Autowired(required = false)
	private Integer hl7ScreenItPort;

	@Qualifier("hl7IlmPort")
	@Autowired(required = false)
	private Integer hl7ScreenItIlmPort;

	@Autowired
	private SimplePreferenceService preferenceService;

	private void verversIMSConfiguratie()
	{
		hl7IMSHost = preferenceService.getString(PreferenceKey.MAMMA_IMS_HOST_NAME.name());
		hl7IMSOrmPort = Integer.valueOf(preferenceService.getString(PreferenceKey.MAMMA_IMS_ORM_PORT.name()));
		hl7IMSAdtPort = Integer.valueOf(preferenceService.getString(PreferenceKey.MAMMA_IMS_ADT_PORT.name()));
	}

	private Message parseMessage(String message)
	{
		try
		{
			Parser p = context.getPipeParser();
			return p.parse(message);
		}
		catch (Exception e)
		{
			LOG.error("Er ging iets mis met parsen van het bericht", e);
		}
		return null;
	}

	@Override
	public ScreenITResponseV251MessageWrapper verstuurIFobtTestBericht(String message) throws LLPException, IOException, HL7Exception
	{
		ScreenITResponseV251MessageWrapper wrapper = null;
		Connection conn = getIfobtConnection();

		Message hapiMsg = parseMessage(message);
		if (conn != null)
		{
			Message response = conn.getInitiator().sendAndReceive(hapiMsg);
			wrapper = new ScreenITResponseV251MessageWrapper(response);
			LOG.info("Message verstuurd. Response was: " + wrapper.getAcknowledgmentCode());
		}

		closingConnection(conn);
		return wrapper;
	}

	@Override
	public ScreenITResponseV24MessageWrapper verstuurORMTestBericht(String message) throws LLPException, IOException, HL7Exception
	{
		verversIMSConfiguratie();
		Connection connection = getImsOrmConnection();
		return sendHL7v24Message(message, connection);
	}

	@Override
	public ScreenITResponseV24MessageWrapper verstuurADTTestBericht(String message) throws LLPException, IOException, HL7Exception
	{
		verversIMSConfiguratie();
		Connection connection = getImsAdtConnection();
		return sendHL7v24Message(message, connection);
	}

	@Override
	public ScreenITResponseV24MessageWrapper verstuurORMTestBerichtNaarScreenIT(String message) throws LLPException, IOException, HL7Exception
	{
		verversIMSConfiguratie();
		Connection connection = getScreenITOrmConnection();
		return sendHL7v24Message(message, connection);
	}

	@Override
	public ScreenITResponseV24MessageWrapper verstuurILMTestBerichtNaarScreenIT(String message) throws LLPException, IOException, HL7Exception
	{
		verversIMSConfiguratie();
		Connection connection = getScreenITOrmIlmConnection();
		return sendHL7v24Message(message, connection);
	}

	private ScreenITResponseV24MessageWrapper sendHL7v24Message(String message, Connection connection) throws LLPException, IOException, HL7Exception
	{
		LOG.info("HL7v24 bericht versturen naar {}:{}", connection.getRemoteAddress().getHostName(), connection.getRemotePort());
		ScreenITResponseV24MessageWrapper wrapper = null;
		Message hapiMsg = parseMessage(message);
		if (connection != null)
		{
			Message response = connection.getInitiator().sendAndReceive(hapiMsg);
			wrapper = new ScreenITResponseV24MessageWrapper(response);
			LOG.info("Message verstuurd. Response was: {}", wrapper.getAcknowledgmentCode());
		}

		closingConnection(connection);
		return wrapper;
	}

	private Connection getIfobtConnection()
	{
		return getConnection(hl7wsbHost, hl7IfobtPort);
	}

	private Connection getImsAdtConnection()
	{
		return getConnection(hl7IMSHost, hl7IMSAdtPort);
	}

	private Connection getScreenITOrmConnection()
	{
		return getConnection(hl7wsbHost, hl7ScreenItPort);
	}

	private Connection getScreenITOrmIlmConnection()
	{
		return getConnection(hl7wsbHost, hl7ScreenItIlmPort);
	}

	private Connection getImsOrmConnection()
	{
		return getConnection(hl7IMSHost, hl7IMSOrmPort);
	}

	private Connection getConnection(String host, Integer port)
	{
		Connection conn = null;
		try
		{
			LOG.info("Poging tot connectie, ");
			conn = context.newClient(host, port, false);
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
