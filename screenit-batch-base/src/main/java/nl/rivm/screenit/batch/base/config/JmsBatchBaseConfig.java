package nl.rivm.screenit.batch.base.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import nl.rivm.screenit.batch.jms.listener.JMSJobStatusListener;
import nl.rivm.screenit.batch.jms.listener.JMSQuartzListener;
import nl.rivm.screenit.batch.jms.listener.JMSStartJobListener;
import nl.rivm.screenit.config.JmsConfig;

import org.quartz.Scheduler;
import org.springframework.batch.core.launch.JobOperator;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.jms.core.JmsTemplate;
import org.springframework.jms.listener.DefaultMessageListenerContainer;

@Configuration
@Profile("!test")
public class JmsBatchBaseConfig
{
	private final JmsConfig jmsConfig;

	private final BatchApplicationConfig batchApplicationConfig;

	public JmsBatchBaseConfig(JmsConfig jmsConfig, BatchApplicationConfig batchApplicationConfig)
	{
		this.jmsConfig = jmsConfig;
		this.batchApplicationConfig = batchApplicationConfig;
	}

	@Bean
	public JMSStartJobListener startJobMessageListener()
	{
		return new JMSStartJobListener(batchApplicationConfig.batchApplicationType());
	}

	@Bean
	public JMSJobStatusListener jmsJobStatusListener(JobOperator jobOperator, JmsTemplate jmsTemplate, Scheduler scheduler)
	{
		return new JMSJobStatusListener(jobOperator, jmsTemplate, scheduler);
	}

	@Bean
	public DefaultMessageListenerContainer jobStatusJmsContainer(JMSJobStatusListener jmsJobStatusListener)
	{
		var listener = new DefaultMessageListenerContainer();
		listener.setConnectionFactory(jmsConfig.jmsFactory());
		listener.setDestination(jmsConfig.batchServerStatusDestination());
		listener.setMessageListener(jmsJobStatusListener);
		listener.setSessionTransacted(true);
		return listener;
	}

	@Bean
	public DefaultMessageListenerContainer quartzJmsContainer(JMSQuartzListener jmsQuartzListener)
	{
		var listener = new DefaultMessageListenerContainer();
		listener.setConnectionFactory(jmsConfig.jmsFactory());
		listener.setDestination(jmsConfig.quartzDestination());
		listener.setMessageListener(jmsQuartzListener);
		listener.setSessionTransacted(true);
		return listener;
	}

}
