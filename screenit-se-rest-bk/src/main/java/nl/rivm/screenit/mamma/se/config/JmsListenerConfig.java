package nl.rivm.screenit.mamma.se.config;

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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.config.JmsConfig;
import nl.rivm.screenit.mamma.se.jms.listener.SeRestMessageListener;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.jms.listener.DefaultMessageListenerContainer;

@Configuration
@Profile("!test")
@AllArgsConstructor
public class JmsListenerConfig
{

	private final JmsConfig jmsConfig;

	@Bean
	public DefaultMessageListenerContainer verwerkMammaSeRestListenerContainer(SeRestMessageListener listener)
	{
		var listenerContainer = new DefaultMessageListenerContainer();
		listenerContainer.setConcurrentConsumers(1);
		listenerContainer.setConnectionFactory(jmsConfig.jmsFactory());
		listenerContainer.setDestination(jmsConfig.verwerkMammaSeRestDestination());
		listenerContainer.setMessageListener(listener);
		listenerContainer.setSessionTransacted(true);
		return listenerContainer;
	}

}
