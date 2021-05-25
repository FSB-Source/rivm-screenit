package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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
import java.util.concurrent.TimeUnit;

import nl.rivm.screenit.batch.model.HL7v24ResponseWrapper;
import nl.rivm.screenit.batch.model.ScreenITHL7MessageContext;
import nl.rivm.screenit.batch.service.HL7BaseSendMessageService;
import nl.rivm.screenit.model.berichten.ScreenITResponseV24MessageWrapper;
import nl.rivm.screenit.service.TechnischeBerichtenLoggingSaverService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import ca.uhn.hl7v2.AcknowledgmentCode;
import ca.uhn.hl7v2.DefaultHapiContext;
import ca.uhn.hl7v2.HL7Exception;
import ca.uhn.hl7v2.app.Connection;
import ca.uhn.hl7v2.app.Initiator;
import ca.uhn.hl7v2.llp.LLPException;
import ca.uhn.hl7v2.model.Message;
import ca.uhn.hl7v2.parser.Parser;

@Service
public class HL7BaseSendMessageServiceImpl implements HL7BaseSendMessageService
{

	private static final Logger LOG = LoggerFactory.getLogger(HL7BaseSendMessageServiceImpl.class);

	@Autowired
	private TechnischeBerichtenLoggingSaverService technischeBerichtenLoggingSaverService;

	@Override
	public void discardConnection(ScreenITHL7MessageContext messageContext)
	{
		ScreenitHapiContext.getHapiContext(messageContext.getType()).getConnectionHub().discard(messageContext.getConnection());
	}

	@Override
	public void openConnection(String connectieNaam, String host, String port, int pogingen, ScreenITHL7MessageContext messageContext) throws HL7Exception
	{
		Connection connection = null;
		for (int t = 0; t < pogingen; t++)
		{
			try
			{
				LOG.info("Verbinding met {}, adres {}:{}, wordt geprobeerd op te zetten (poging {}).", connectieNaam, host, port, t + 1);
				connection = ScreenitHapiContext.getHapiContext(messageContext.getType()).newClient(host, Integer.valueOf(port), false);
				messageContext.setConnection(connection);
				Initiator init = connection.getInitiator();
				init.setTimeout(30L, TimeUnit.SECONDS);
				LOG.trace("Verbinding met hl7 connectie {} is opgezet. {}:{}", connectieNaam, host, port);
				break;
			}
			catch (HL7Exception e)
			{
				LOG.error("Exceptie bij maken hl7v24 connectie", e);
				connection = null;
			}
		}
		if (connection == null)
		{
			String melding = "Er kon geen connectie worden gemaakt met hl7 connectie: " + connectieNaam + ", (" + host + ":" + port + ")";
			messageContext.getResponseWrapper().setMelding(melding);
			throw new HL7Exception(melding);
		}
	}

	@Override
	public HL7v24ResponseWrapper sendHL7Message(String hl7Bericht, ScreenITHL7MessageContext messageContext) throws HL7Exception, LLPException, IOException
	{
		DefaultHapiContext hapiContext = ScreenitHapiContext.getHapiContext(messageContext.getType());

		Parser parser = hapiContext.getPipeParser();
		Message hapiMsg;
		synchronized (parser)
		{
			hapiMsg = parser.parse(hl7Bericht);
		}
		return sendHL7Message(hapiMsg, messageContext);
	}

	@Override
	public HL7v24ResponseWrapper sendHL7Message(Message hl7Bericht, ScreenITHL7MessageContext messageContext) throws HL7Exception, LLPException, IOException
	{

		long exchangeId = technischeBerichtenLoggingSaverService.logRequest("HL7V2_REQ_OUT", hl7Bericht.getClass().getSimpleName(), hl7Bericht.toString());

		HL7v24ResponseWrapper responseWrapper = messageContext.getResponseWrapper();

		String responseType = "HL7V2_RESP_IN";
		String rawResponse = null;
		try
		{
			Message response = messageContext.getConnection().getInitiator().sendAndReceive(hl7Bericht);
			ScreenITResponseV24MessageWrapper responseV24MessageWrapper = new ScreenITResponseV24MessageWrapper(response);
			responseWrapper.setResponseV24MessageWrapper(responseV24MessageWrapper);
			AcknowledgmentCode acknowledgmentCode = responseV24MessageWrapper.getAcknowledgmentCode();
			responseWrapper.setSuccess(AcknowledgmentCode.AA == acknowledgmentCode || AcknowledgmentCode.CA == acknowledgmentCode);
			if (!responseWrapper.isSuccess())
			{
				responseWrapper.setMelding("Onverwachte acknowledgment code '" + acknowledgmentCode + "' ipv 'AA' of 'CA'.");
				LOG.error(responseWrapper.getMelding());
			}
			else
			{
				LOG.info("Message verstuurd. Response was: " + acknowledgmentCode);
			}
			rawResponse = response.toString();
		}
		catch (Exception e)
		{
			responseType = "HL7V2_FAULT_IN";
			responseWrapper.setMelding(e.getMessage());
			responseWrapper.setCrashException(e);
			throw e;
		}
		finally
		{
			technischeBerichtenLoggingSaverService.logResponse(responseType, exchangeId, rawResponse);
		}

		return responseWrapper;
	}
}
