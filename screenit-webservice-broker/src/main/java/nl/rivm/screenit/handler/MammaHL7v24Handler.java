package nl.rivm.screenit.handler;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
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

import nl.rivm.screenit.wsb.service.mamma.MammaHL7v24Service;
import nl.topicuszorg.spring.injection.SpringBeanProvider;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import ca.uhn.hl7v2.HL7Exception;
import ca.uhn.hl7v2.app.ApplicationException;
import ca.uhn.hl7v2.model.Message;
import ca.uhn.hl7v2.model.v24.message.ORM_O01;

public class MammaHL7v24Handler extends AbstractHL7v2Handler<ORM_O01>
{
	private static final Logger LOG = LoggerFactory.getLogger(MammaHL7v24Handler.class);

	@Autowired
	private MammaHL7v24Service mammaHL7V24Service;

	public MammaHL7v24Handler(Class<ORM_O01> berichtType, Class<? extends MammaHL7v24Service> clazz)
	{
		super(berichtType);
		this.mammaHL7V24Service = SpringBeanProvider.getInstance().getBean(clazz);
	}

	@Override
	protected Message verwerkTypedMessage(ORM_O01 message) throws ApplicationException, HL7Exception
	{
		return mammaHL7V24Service.processTypedMessage(message);
	}

	@Override
	public Message processTypedMessage(ORM_O01 message) throws ApplicationException, HL7Exception
	{
		synchronized (this)
		{
			return super.processTypedMessage(message);
		}
	}
}
