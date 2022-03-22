package nl.rivm.screenit.mamma.planning.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import javax.sql.DataSource;

import nl.topicuszorg.hibernate.spring.util.sessionfactory.TopicusPostConfigurationSessionFactoryBean;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.DependsOn;
import org.springframework.orm.hibernate5.HibernateTransactionManager;

@Configuration
public class TransactionManagerConfig
{

	@Autowired
	private DataSource dataSource;

	@Autowired
	private TopicusPostConfigurationSessionFactoryBean hibernateSessionFactory;

	@Bean
	@DependsOn({ "dataSource", "hibernateSessionFactory" })
	public HibernateTransactionManager transactionManager()
	{
		final HibernateTransactionManager transactionManager = new HibernateTransactionManager();
		transactionManager.setDataSource(dataSource);
		transactionManager.setSessionFactory(hibernateSessionFactory.getObject());
		return transactionManager;
	}

}
