package nl.rivm.screenit.mamma.se.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.time.Duration;

import nl.rivm.screenit.mamma.se.websocket.socket.SeProxyWebsocket;
import nl.rivm.screenit.service.impl.DefaultCurrentDateSupplier;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service(value = "currentDateSuplier")
public class SeCurrentDateSupplier extends DefaultCurrentDateSupplier
{

	@Autowired
	private SeProxyWebsocket seProxyWebsocket;

	@Override
	public void setOffset(Duration offset)
	{
		super.setOffset(offset);
		seProxyWebsocket.sendTijdUpdateNaarIedereSe(offset);
	}
}
