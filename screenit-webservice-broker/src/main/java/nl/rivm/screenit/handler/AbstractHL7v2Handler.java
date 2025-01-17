package nl.rivm.screenit.handler;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.concurrent.atomic.AtomicReference;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.service.TechnischeBerichtenLoggingSaverService;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5Session;
import nl.topicuszorg.hibernate.spring.util.ApplicationContextProvider;
import nl.topicuszorg.hl7v2.services.server.impl.TypedHL7BerichtTypeHandlerImpl;

import ca.uhn.hl7v2.HL7Exception;
import ca.uhn.hl7v2.app.ApplicationException;
import ca.uhn.hl7v2.model.Message;
import ca.uhn.hl7v2.model.v24.datatype.MSG;
import ca.uhn.hl7v2.model.v24.segment.MSH;

@Slf4j
public abstract class AbstractHL7v2Handler<T extends Message> extends TypedHL7BerichtTypeHandlerImpl<T>
{
	private final TechnischeBerichtenLoggingSaverService technischeBerichtenLoggingSaverService;

	protected AbstractHL7v2Handler(Class<T> berichtType)
	{
		super(berichtType);
		this.technischeBerichtenLoggingSaverService = ApplicationContextProvider.getApplicationContext().getBean(TechnischeBerichtenLoggingSaverService.class);
	}

	@Override
	public Message processTypedMessage(T message) throws ApplicationException, HL7Exception
	{
		AtomicReference<Message> response = new AtomicReference<>();
		OpenHibernate5Session.withoutTransaction().run(() ->
			{
				long exchangeId = technischeBerichtenLoggingSaverService.logRequest("HL7V2_REQ_IN", getBerichtType().getSimpleName(), message.toString());
				response.set(verwerkTypedMessage(message));
				technischeBerichtenLoggingSaverService.logResponse("HL7V2_RESP_OUT", exchangeId, response.toString());
			}
		);
		return response.get();

	}

	abstract Message verwerkTypedMessage(T message) throws ApplicationException, HL7Exception;

	@Override
	public boolean canProcess(Message arg0)
	{
		try
		{
			if (arg0.get("MSH") instanceof ca.uhn.hl7v2.model.v251.segment.MSH)
			{
				return checkHL7v251Header(arg0);
			}
			else if (arg0.get("MSH") instanceof MSH)
			{
				return checkHL7v24Header(arg0);
			}
		}
		catch (HL7Exception e)
		{
			LOG.error(e.getMessage(), e);
		}
		return false;
	}

	private boolean checkHL7v24Header(Message arg0) throws HL7Exception
	{
		MSH msh = (MSH) arg0.get("MSH");
		MSG messageType = msh.getMessageType();
		return messageType.getMessageType().getValue().equals(getMessageType())
			&& messageType.getTriggerEvent().getValue().equals(getTriggerEvent());
	}

	private boolean checkHL7v251Header(Message arg0) throws HL7Exception
	{
		ca.uhn.hl7v2.model.v251.segment.MSH msh = (ca.uhn.hl7v2.model.v251.segment.MSH) arg0.get("MSH");

		ca.uhn.hl7v2.model.v251.datatype.MSG messageType = msh.getMessageType();
		return messageType.getMessageCode().getValue().equals(getMessageType()) && messageType.getTriggerEvent().getValue().equals(getTriggerEvent());
	}
}
