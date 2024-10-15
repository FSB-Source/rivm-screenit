package nl.rivm.screenit.config;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import javax.sql.DataSource;

import nl.rivm.screenit.cache.JNDIJGroupsCacheManagerPeerProviderFactory;
import nl.rivm.screenit.util.query.ExtractYearMetadataBuilderContributor;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.dao.impl.HibernateSearchServiceImpl;
import nl.topicuszorg.hibernate.spring.dao.impl.HibernateServiceImpl;
import nl.topicuszorg.hibernate.spring.util.naming.ImplicitHibernate4LegacyNamingStrategy;
import nl.topicuszorg.hibernate.spring.util.naming.PhysicalHibernate4LegacyNamingStrategy;
import nl.topicuszorg.hibernate.spring.util.sessionfactory.TopicusPostConfigurationSessionFactoryBean;

import org.hibernate.boot.spi.MetadataBuilderContributor;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.orm.hibernate5.HibernateTransactionManager;
import org.springframework.orm.jpa.support.SharedEntityManagerBean;
import org.springframework.transaction.annotation.EnableTransactionManagement;
import org.springframework.transaction.support.AbstractPlatformTransactionManager;

import com.zaxxer.hikari.HikariDataSource;

@Configuration
@EnableTransactionManagement
public class BaseHibernateConfig
{

	@ConfigurationProperties(prefix = "spring.datasource.hikari")
	@Bean
	@Profile("!cucumber & !test")
	public HikariDataSource dataSource()
	{
		return new HikariDataSource();
	}

	@Bean(name = { "entityManagerFactory", "hibernateSessionFactory" })
	@Profile("!test & !cucumber")
	public TopicusPostConfigurationSessionFactoryBean hibernateSessionFactory(DataSource dataSource, ApplicationConfig applicationConfig, JGroupsConfig jGroupsConfig,
		Optional<List<Resource>> additionalHibernateConfigLocations)
	{
		JNDIJGroupsCacheManagerPeerProviderFactory.applicationInstance = applicationConfig.applicationInstance();
		JNDIJGroupsCacheManagerPeerProviderFactory.bindIp = jGroupsConfig.jgroupsBindIP();
		JNDIJGroupsCacheManagerPeerProviderFactory.bindPort = jGroupsConfig.jgroupsEhcacheBindPort();
		JNDIJGroupsCacheManagerPeerProviderFactory.initialHosts = jGroupsConfig.jgroupsEhcacheIPPorts();

		final TopicusPostConfigurationSessionFactoryBean hibernateSessionFactory = new TopicusPostConfigurationSessionFactoryBean();

		List<Resource> configLocations = new ArrayList<>(Arrays.asList(
			new ClassPathResource("hibernate.cfg.xml"),
			new ClassPathResource("hibernate-spring-boot.cfg.xml"),
			new ClassPathResource("hibernate-dataset-mapping.cfg.xml"),
			new ClassPathResource("hibernate-mapping.cfg.xml"),
			new ClassPathResource("hibernate-organisatie.cfg.xml"),
			new ClassPathResource("hibernate-persoonsgegevens.cfg.xml"),
			new ClassPathResource("hibernate-formulieren2.cfg.xml")));
		additionalHibernateConfigLocations.ifPresent(configLocations::addAll);
		hibernateSessionFactory.setConfigLocations(configLocations.toArray(Resource[]::new));

		hibernateSessionFactory.setDataSource(dataSource);

		hibernateSessionFactory.setPhysicalNamingStrategy(new PhysicalHibernate4LegacyNamingStrategy());
		hibernateSessionFactory.setImplicitNamingStrategy(new ImplicitHibernate4LegacyNamingStrategy());

		return hibernateSessionFactory;
	}

	@Bean
	@Profile("!test")
	public HibernateService hibernateService(TopicusPostConfigurationSessionFactoryBean hibernateSessionFactory)
	{
		final HibernateServiceImpl hibernateService = new HibernateServiceImpl();
		hibernateService.setSessionFactory(hibernateSessionFactory.getObject());
		return hibernateService;
	}

	@Bean
	@Profile("!test")
	public HibernateSearchServiceImpl hibernateSearchService(TopicusPostConfigurationSessionFactoryBean hibernateSessionFactory)
	{
		final HibernateSearchServiceImpl hibernateSearchService = new HibernateSearchServiceImpl();
		hibernateSearchService.setSessionFactory(hibernateSessionFactory.getObject());
		return hibernateSearchService;
	}

	@Bean
	public HibernateTransactionManager transactionManager(TopicusPostConfigurationSessionFactoryBean hibernateSessionFactory)
	{
		final HibernateTransactionManager transactionManager = new HibernateTransactionManager();
		transactionManager.setSessionFactory(hibernateSessionFactory.getObject());
		transactionManager.setTransactionSynchronization(AbstractPlatformTransactionManager.SYNCHRONIZATION_ON_ACTUAL_TRANSACTION);
		return transactionManager;
	}

	@Bean
	public SharedEntityManagerBean sharedEntityManager(TopicusPostConfigurationSessionFactoryBean hibernateSessionFactory)
	{
		SharedEntityManagerBean sharedEntityManager = new SharedEntityManagerBean();
		sharedEntityManager.setEntityManagerFactory(hibernateSessionFactory.getObject());
		return sharedEntityManager;
	}

	@Bean
	public MetadataBuilderContributor extractYearFunctionContributor()
	{
		return new ExtractYearMetadataBuilderContributor();
	}
}
