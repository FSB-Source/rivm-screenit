package nl.rivm.screenit.huisartsenportaal.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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

import java.util.ArrayList;
import java.util.List;

import javax.jms.ConnectionFactory;

import org.apache.activemq.ActiveMQConnectionFactory;
import org.apache.activemq.ActiveMQPrefetchPolicy;
import org.apache.activemq.RedeliveryPolicy;
import org.apache.activemq.jms.pool.PooledConnectionFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.jms.config.DefaultJmsListenerContainerFactory;
import org.springframework.jms.config.JmsListenerContainerFactory;
import org.springframework.jms.core.JmsTemplate;

@Configuration
public class JmsConfigHuisartsPortaal
{
	@Value("${app.activemqUsername}")
	private String activemqUsername;

	@Value("${app.activemqPassword}")
	private String activemqPassword;

	@Value("${spring.activemq.broker-url}")
	private String brokerUrl;

	@Bean
	protected JmsListenerContainerFactory jmsListenerContainerFactory()
	{
		DefaultJmsListenerContainerFactory containerFactory = new DefaultJmsListenerContainerFactory();
		containerFactory.setConnectionFactory(pooledConnectionFactory(connectionFactory()));
		return containerFactory;
	}

	@Bean(destroyMethod = "stop", initMethod = "start")
	protected PooledConnectionFactory pooledConnectionFactory(ConnectionFactory connectionFactory)
	{
		PooledConnectionFactory jmsFactory = new PooledConnectionFactory();
		jmsFactory.setConnectionFactory(connectionFactory);
		jmsFactory.setIdleTimeout(0);
		return jmsFactory;
	}

	@Bean
	protected ConnectionFactory connectionFactory()
	{
		ActiveMQConnectionFactory connectionFactory = new ActiveMQConnectionFactory();
		connectionFactory.setUseAsyncSend(true);
		connectionFactory.setPrefetchPolicy(prefetchPolicy());
		connectionFactory.setRedeliveryPolicy(redeliveryPolicy());
		connectionFactory.setBrokerURL(brokerUrl);

		if (!activemqUsername.isBlank() && !activemqPassword.isBlank())
		{
			connectionFactory.setUserName(activemqUsername);
			connectionFactory.setPassword(activemqPassword);
		}
		List<String> trustedPackages = new ArrayList<>();
		trustedPackages.add("nl.rivm.screenit");
		trustedPackages.add("java");
		connectionFactory.setTrustedPackages(trustedPackages);

		return connectionFactory;
	}

	@Bean
	RedeliveryPolicy redeliveryPolicy()
	{
		RedeliveryPolicy policy = new RedeliveryPolicy();
		policy.setMaximumRedeliveries(3);
		policy.setInitialRedeliveryDelay(3000);
		return policy;
	}

	@Bean
	ActiveMQPrefetchPolicy prefetchPolicy()
	{
		ActiveMQPrefetchPolicy policy = new ActiveMQPrefetchPolicy();
		policy.setQueuePrefetch(1);
		return policy;
	}

	@Bean
	JmsTemplate jmsTemplate()
	{
		JmsTemplate template = new JmsTemplate();
		template.setConnectionFactory(connectionFactory());
		return template;
	}
}
