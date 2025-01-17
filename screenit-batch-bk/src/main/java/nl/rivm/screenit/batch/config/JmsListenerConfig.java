package nl.rivm.screenit.batch.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import nl.rivm.screenit.batch.base.config.JmsBatchBaseConfig;
import nl.rivm.screenit.batch.jms.listener.JMSUploadBeeldenBerichtListener;
import nl.rivm.screenit.batch.jms.listener.JMSVerwerkCdaBerichtListener;
import nl.rivm.screenit.batch.jms.listener.JMSVerwerkIMSBerichtListener;
import nl.rivm.screenit.batch.jms.listener.JMSVerzamelOnderzoekDataBerichtListener;
import nl.rivm.screenit.config.JmsConfig;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.jms.listener.DefaultMessageListenerContainer;

@Configuration
@Profile("!test")
public class JmsListenerConfig
{
	private final JmsConfig jmsConfig;

	private final JmsBatchBaseConfig jmsBatchBaseConfig;

	public JmsListenerConfig(JmsConfig jmsConfig, JmsBatchBaseConfig jmsBatchBaseConfig)
	{
		this.jmsConfig = jmsConfig;
		this.jmsBatchBaseConfig = jmsBatchBaseConfig;
	}

	@Bean
	public JMSVerwerkCdaBerichtListener verwerkMammaCdaBerichtListener()
	{
		var listener = new JMSVerwerkCdaBerichtListener();
		listener.setBvo(Bevolkingsonderzoek.MAMMA);
		return listener;
	}

	@Bean
	public DefaultMessageListenerContainer mammaJobJmsContainer()
	{
		var listenerContainer = new DefaultMessageListenerContainer();
		listenerContainer.setConnectionFactory(jmsConfig.jmsFactory());
		listenerContainer.setDestination(jmsConfig.mammaJobsDestination());
		listenerContainer.setMessageListener(jmsBatchBaseConfig.startJobMessageListener());
		listenerContainer.setSessionTransacted(true);
		return listenerContainer;
	}

	@Bean
	public DefaultMessageListenerContainer verwerkIMSBerichtListenerContainer(JMSVerwerkIMSBerichtListener verwerkIMSBerichtListener)
	{
		var listenerContainer = new DefaultMessageListenerContainer();
		listenerContainer.setConcurrentConsumers(1);
		listenerContainer.setConnectionFactory(jmsConfig.jmsFactory());
		listenerContainer.setDestination(jmsConfig.verwerkMammaIMSBerichtDestination());
		listenerContainer.setMessageListener(verwerkIMSBerichtListener);
		listenerContainer.setSessionTransacted(true);
		return listenerContainer;
	}

	@Bean
	public DefaultMessageListenerContainer verzamelOnderzoekDataBerichtListenerContainer(JMSVerzamelOnderzoekDataBerichtListener verzamelOnderzoekDataBerichtListener)
	{
		var listenerContainer = new DefaultMessageListenerContainer();
		listenerContainer.setConcurrentConsumers(1);
		listenerContainer.setConnectionFactory(jmsConfig.jmsFactory());
		listenerContainer.setDestination(jmsConfig.verzamelOnderzoekDataBerichtDestination());
		listenerContainer.setMessageListener(verzamelOnderzoekDataBerichtListener);
		listenerContainer.setSessionTransacted(true);
		return listenerContainer;
	}

	@Bean
	public DefaultMessageListenerContainer uploadBeeldenVerzoekBerichtListenerContainer(JMSUploadBeeldenBerichtListener uploadBeeldenBerichtListener)
	{
		var listenerContainer = new DefaultMessageListenerContainer();
		listenerContainer.setConcurrentConsumers(1);
		listenerContainer.setConnectionFactory(jmsConfig.jmsFactory());
		listenerContainer.setDestination(jmsConfig.uploadBeeldenVerzoekBerichtDestination());
		listenerContainer.setMessageListener(uploadBeeldenBerichtListener);
		listenerContainer.setSessionTransacted(true);
		return listenerContainer;
	}

	@Bean
	public DefaultMessageListenerContainer verwerkCdaBerichtListenerContainer(JMSVerwerkCdaBerichtListener verwerkMammaCdaBerichtListener)
	{
		var listenerContainer = new DefaultMessageListenerContainer();
		listenerContainer.setConcurrentConsumers(1);
		listenerContainer.setConnectionFactory(jmsConfig.jmsFactory());
		listenerContainer.setDestination(jmsConfig.verwerkMammaCdaBerichtDestination());
		listenerContainer.setMessageListener(verwerkMammaCdaBerichtListener);
		listenerContainer.setSessionTransacted(true);
		return listenerContainer;
	}
}
