package nl.rivm.screenit.clientportaal.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Arrays;

import javax.jms.Destination;

import org.apache.activemq.ActiveMQConnectionFactory;
import org.apache.activemq.ActiveMQPrefetchPolicy;
import org.apache.activemq.RedeliveryPolicy;
import org.apache.activemq.command.ActiveMQQueue;
import org.apache.activemq.pool.PooledConnectionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.jms.core.JmsTemplate;

@Configuration
public class JmsConfig
{

	@Autowired
	private ApplicationConfig appConfig;

	@Bean
	public Destination verwerkColonCdaBerichtDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.colon.cdaberichten." + appConfig.applicationEnvironment());
	}

	@Bean
	public Destination verwerkCervixCdaBerichtDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.cervix.cdaberichten." + appConfig.applicationEnvironment());
	}

	@Bean
	public Destination verwerkMammaCdaBerichtDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.mamma.cdaberichten." + appConfig.applicationEnvironment());
	}

	@Bean
	public Destination verwerkMammaIMSBerichtDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.mamma.hl7berichten." + appConfig.applicationEnvironment());
	}

	@Bean
	public Destination verzamelOnderzoekDataBerichtDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.mamma.verzamelondezoeksdata." + appConfig.applicationEnvironment());
	}

	@Bean
	public Destination uploadBeeldenVerzoekBerichtDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.mamma.uploadbeeldenverzoek." + appConfig.applicationEnvironment());
	}

	@Bean
	public Destination verwerkHpvBerichtDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.cervix.hpvberichten." + appConfig.applicationEnvironment());
	}

	@Bean
	public Destination verwerkIFobtBerichtDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.colon.ifobtberichten." + appConfig.applicationEnvironment());
	}

	@Bean
	public Destination verwerkBulkHuisartsenDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.cervix.bulkhuisarsen." + appConfig.applicationEnvironment());
	}

	@Bean
	public Destination verwerkMammaSeRestDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.mamma.se.jms.listener." + appConfig.applicationEnvironment());
	}

	@Bean
	public Destination colonJobsDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.colon.jobs." + appConfig.applicationEnvironment());
	}

	@Bean
	public Destination generalisJobsDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.generalis.jobs." + appConfig.applicationEnvironment());
	}

	@Bean
	public Destination cervixJobsDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.cervix.jobs." + appConfig.applicationEnvironment());
	}

	@Bean
	public Destination mammaJobsDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.mamma.jobs." + appConfig.applicationEnvironment());
	}

	@Bean
	public Destination huisartsporaalDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.huisartsportaal.listener." + appConfig.applicationEnvironment());
	}

	@Bean
	public Destination huisartsportaalListenerDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.main.jms.listener.huisartsportaallistener." + appConfig.applicationEnvironment());
	}

	@Bean
	public Destination batchServerStatusDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.batchServerStatus." + appConfig.applicationEnvironment());
	}

	@Bean
	public Destination quartzDestination()
	{
		return new ActiveMQQueue("nl.rivm.screenit.batch.quartz." + appConfig.applicationEnvironment());
	}

	@Bean
	public ActiveMQPrefetchPolicy prefetchPolicy()
	{
		final ActiveMQPrefetchPolicy prefetchPolicy = new ActiveMQPrefetchPolicy();
		prefetchPolicy.setQueuePrefetch(1);
		return prefetchPolicy;
	}

	@Bean
	public RedeliveryPolicy redeliveryPolicy()
	{
		final RedeliveryPolicy redeliveryPolicy = new RedeliveryPolicy();
		redeliveryPolicy.setMaximumRedeliveries(3);
		redeliveryPolicy.setInitialRedeliveryDelay(3000);
		return redeliveryPolicy;
	}

	@Bean
	public ActiveMQConnectionFactory activeMQConnectionFactory()
	{
		final ActiveMQConnectionFactory connectionFactory = new ActiveMQConnectionFactory();
		connectionFactory.setBrokerURL(appConfig.jmsBrokerUrl());
		connectionFactory.setUseAsyncSend(true);
		connectionFactory.setPrefetchPolicy(prefetchPolicy());
		connectionFactory.setRedeliveryPolicy(redeliveryPolicy());
		connectionFactory.setTrustedPackages(
			Arrays.asList("org.quartz.utils", "nl.rivm.screenit.model.batch", "nl.rivm.screenit.huisartsenportaal.dto", "nl.rivm.screenit.model.enums", "java.lang", "java.util"));
		return connectionFactory;
	}

	@Bean(initMethod = "start", destroyMethod = "stop")
	public PooledConnectionFactory jmsFactory()
	{
		final PooledConnectionFactory jmsFactory = new PooledConnectionFactory();
		jmsFactory.setConnectionFactory(activeMQConnectionFactory());
		jmsFactory.setIdleTimeout(0);
		return jmsFactory;
	}

	@Bean
	public JmsTemplate jmsTemplate()
	{
		final JmsTemplate jmsTemplate = new JmsTemplate();
		jmsTemplate.setConnectionFactory(activeMQConnectionFactory());
		return jmsTemplate;
	}

}
