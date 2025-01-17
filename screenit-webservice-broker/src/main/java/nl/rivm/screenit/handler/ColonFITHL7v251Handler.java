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

import nl.rivm.screenit.wsb.service.colon.ColonFITHL7v251Service;
import nl.topicuszorg.hibernate.spring.util.ApplicationContextProvider;

import ca.uhn.hl7v2.HL7Exception;
import ca.uhn.hl7v2.app.ApplicationException;
import ca.uhn.hl7v2.model.Message;
import ca.uhn.hl7v2.model.v251.message.OUL_R22;

public class ColonFITHL7v251Handler extends AbstractHL7v2Handler<OUL_R22>
{

	private ColonFITHL7v251Service fitHL7V251Service;

	public ColonFITHL7v251Handler(Class<OUL_R22> berichtType)
	{
		super(berichtType);
		this.fitHL7V251Service = ApplicationContextProvider.getApplicationContext().getBean(ColonFITHL7v251Service.class);
	}

	@Override
	public Message verwerkTypedMessage(OUL_R22 message) throws ApplicationException, HL7Exception
	{
		return fitHL7V251Service.processTypedMessage(message);
	}

}
