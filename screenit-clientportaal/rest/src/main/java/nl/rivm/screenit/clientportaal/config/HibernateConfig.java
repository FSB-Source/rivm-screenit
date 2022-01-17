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

import nl.rivm.screenit.cache.JNDIJGroupsCacheManagerPeerProviderFactory;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.dao.impl.HibernateSearchServiceImpl;
import nl.topicuszorg.hibernate.spring.dao.impl.HibernateServiceImpl;
import nl.topicuszorg.hibernate.spring.util.naming.ImplicitHibernate4LegacyNamingStrategy;
import nl.topicuszorg.hibernate.spring.util.naming.PhysicalHibernate4LegacyNamingStrategy;
import nl.topicuszorg.hibernate.spring.util.sessionfactory.TopicusPostConfigurationSessionFactoryBean;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.DependsOn;
import org.springframework.core.annotation.Order;
import org.springframework.core.io.ClassPathResource;

import com.zaxxer.hikari.HikariDataSource;

@Configuration
@Order
public class HibernateConfig
{

	@Autowired
	private ApplicationConfig applicationConfig;

	@ConfigurationProperties(prefix = "spring.datasource.hikari")
	@Bean
	public HikariDataSource dataSource()
	{
		return new HikariDataSource();
	}

	@Bean(name = { "entityManagerFactory", "hibernateSessionFactory" })
	@Order(-1)
	@DependsOn({ "jgroupsBindIP", "jgroupsEhcacheBindPort", "jgroupsEhcacheIPPorts" })
	public TopicusPostConfigurationSessionFactoryBean hibernateSessionFactory()
	{
		JNDIJGroupsCacheManagerPeerProviderFactory.applicationInstance = applicationConfig.applicatieInstantie();
		JNDIJGroupsCacheManagerPeerProviderFactory.bindIp = applicationConfig.jgroupsBindIP();
		JNDIJGroupsCacheManagerPeerProviderFactory.bindPort = applicationConfig.jgroupsEhcacheBindPort();
		JNDIJGroupsCacheManagerPeerProviderFactory.initialHosts = applicationConfig.jgroupsEhcacheIPPorts();

		final TopicusPostConfigurationSessionFactoryBean hibernateSessionFactory = new TopicusPostConfigurationSessionFactoryBean();
		hibernateSessionFactory.setConfigLocations(
			new ClassPathResource("hibernate.cfg.xml"),
			new ClassPathResource("hibernate-spring-boot.cfg.xml"),
			new ClassPathResource("hibernate-dataset-mapping.cfg.xml"),
			new ClassPathResource("hibernate-mapping.cfg.xml"),
			new ClassPathResource("hibernate-organisatie.cfg.xml"),
			new ClassPathResource("hibernate-wicket-planning.cfg.xml"),
			new ClassPathResource("hibernate-persoonsgegevens.cfg.xml"),
			new ClassPathResource("hibernate-formulieren2.cfg.xml"));

		hibernateSessionFactory.setDataSource(dataSource());

		hibernateSessionFactory.setPhysicalNamingStrategy(new PhysicalHibernate4LegacyNamingStrategy());
		hibernateSessionFactory.setImplicitNamingStrategy(new ImplicitHibernate4LegacyNamingStrategy());

		return hibernateSessionFactory;
	}

	@Bean
	public HibernateService hibernateService()
	{
		final HibernateServiceImpl hibernateService = new HibernateServiceImpl();
		hibernateService.setSessionFactory(hibernateSessionFactory().getObject());

		return hibernateService;
	}

	@Bean
	public HibernateSearchServiceImpl hibernateSearchService()
	{
		final HibernateSearchServiceImpl hibernateSearchService = new HibernateSearchServiceImpl();
		hibernateSearchService.setSessionFactory(hibernateSessionFactory().getObject());
		return hibernateSearchService;
	}

}
