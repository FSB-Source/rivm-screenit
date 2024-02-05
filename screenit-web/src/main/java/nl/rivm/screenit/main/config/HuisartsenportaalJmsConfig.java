package nl.rivm.screenit.main.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import javax.jms.Destination;

import nl.rivm.screenit.main.jms.listener.HuisartsportaalListener;

import org.apache.activemq.pool.PooledConnectionFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.jms.listener.DefaultMessageListenerContainer;

@Configuration
public class HuisartsenportaalJmsConfig
{

	@Bean
	@Profile("!filler")
	public DefaultMessageListenerContainer huisartsportaalListenerContainer(PooledConnectionFactory jmsFactory, HuisartsportaalListener listener,
		Destination huisartsportaalListenerDestination)
	{
		var container = new DefaultMessageListenerContainer();
		container.setConcurrentConsumers(1);
		container.setConnectionFactory(jmsFactory);
		container.setDestination(huisartsportaalListenerDestination);
		container.setMessageListener(listener);
		container.setSessionTransacted(true);
		return container;
	}

}
