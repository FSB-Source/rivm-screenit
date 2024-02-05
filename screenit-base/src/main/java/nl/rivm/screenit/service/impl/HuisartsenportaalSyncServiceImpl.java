
package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.Serializable;

import javax.jms.Destination;

import nl.rivm.screenit.model.helper.ActiveMQHelper;
import nl.rivm.screenit.service.HuisartsenportaalSyncService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.jms.core.JmsTemplate;
import org.springframework.stereotype.Service;

@Service
public class HuisartsenportaalSyncServiceImpl implements HuisartsenportaalSyncService
{
	@Autowired
	private JmsTemplate jmsTemplate;

	@Autowired
	@Qualifier("huisartsportaalDestination")
	private Destination huisartsportaalDestination;

	@Override
	public void sendJmsBericht(Serializable object)
	{
		jmsTemplate.send(huisartsportaalDestination, session -> ActiveMQHelper.getActiveMqObjectMessage(object));
	}
}
