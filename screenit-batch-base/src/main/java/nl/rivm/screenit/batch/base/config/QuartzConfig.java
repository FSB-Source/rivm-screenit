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

import java.util.Properties;

import javax.sql.DataSource;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.quartz.QuartzDataSource;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.orm.hibernate5.HibernateTransactionManager;
import org.springframework.scheduling.quartz.SchedulerFactoryBean;

@Configuration
@QuartzDataSource
public class QuartzConfig
{

	@Bean
	public SchedulerFactoryBean schedulerFactoryBean(DataSource dataSource, HibernateTransactionManager transactionManager,
		@Qualifier(value = "applicationInstance") String applicationInstance)
	{
		var schedulerFactoryBean = new SchedulerFactoryBean();
		schedulerFactoryBean.setSchedulerName("org.springframework.scheduling.quartz.SchedulerFactoryBean#0"); 
		schedulerFactoryBean.setDataSource(dataSource);
		schedulerFactoryBean.setTransactionManager(transactionManager);
		var properties = new Properties();
		properties.put("org.quartz.scheduler.jmx.export", "true");
		properties.put("org.quartz.scheduler.instanceId", applicationInstance);
		properties.put("org.quartz.jobStore.misfireThreshold", "60000");
		properties.put("org.quartz.jobStore.driverDelegateClass", "org.quartz.impl.jdbcjobstore.PostgreSQLDelegate");
		properties.put("org.quartz.jobStore.isClustered", "true");
		properties.put("org.quartz.jobStore.clusterCheckinInterval", "20000");
		properties.put("org.quartz.jobStore.tablePrefix", "gedeeld.QRTZ_");
		properties.put("org.quartz.threadPool.class", "org.quartz.simpl.SimpleThreadPool");
		properties.put("org.quartz.threadPool.threadCount", "25");
		properties.put("org.quartz.threadPool.threadPriority", "5");
		properties.put("org.quartz.jobStore.class", "org.springframework.scheduling.quartz.LocalDataSourceJobStore");
		schedulerFactoryBean.setQuartzProperties(properties);
		return schedulerFactoryBean;
	}

}
