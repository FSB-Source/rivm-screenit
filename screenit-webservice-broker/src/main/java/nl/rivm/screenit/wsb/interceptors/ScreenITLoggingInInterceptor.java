package nl.rivm.screenit.wsb.interceptors;

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

import org.apache.cxf.ext.logging.LoggingInInterceptor;
import org.apache.cxf.ext.logging.event.LogEvent;
import org.apache.cxf.message.Exchange;
import org.apache.cxf.message.Message;

public class ScreenITLoggingInInterceptor extends LoggingInInterceptor
{
	private final static int LIMIT = 100 * 1024;

	public ScreenITLoggingInInterceptor()
	{
		super(new ScreenITLoggingSaver());
		setLimit(LIMIT);
	}

	@Override
	public void createExchangeId(Message message)
	{
		Exchange exchange = message.getExchange();
		String exchangeId = (String) exchange.get(LogEvent.KEY_EXCHANGE_ID);
		if (exchangeId == null)
		{
			exchangeId = ((ScreenITLoggingSaver) sender).createExchangeId().toString();
			exchange.put(LogEvent.KEY_EXCHANGE_ID, exchangeId);
		}
	}
}
