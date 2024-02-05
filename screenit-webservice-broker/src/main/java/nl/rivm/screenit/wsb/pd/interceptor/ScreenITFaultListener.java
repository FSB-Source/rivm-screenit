package nl.rivm.screenit.wsb.pd.interceptor;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.Constants;

import org.apache.cxf.binding.xml.XMLFault;
import org.apache.cxf.logging.FaultListener;
import org.apache.cxf.message.Message;
import org.springframework.stereotype.Component;

@Component
public class ScreenITFaultListener implements FaultListener
{

	@Override
	public boolean faultOccurred(Exception e, String s, Message message)
	{
		if (e instanceof XMLFault)
		{
			XMLFault xmlFault = (XMLFault) e;
			String faultMessage = xmlFault.getMessage();
			if (faultMessage != null && faultMessage.startsWith(Constants.XML_FAULT_PREFIX))
			{

				return false;
			}
		}
		return true;
	}
}
