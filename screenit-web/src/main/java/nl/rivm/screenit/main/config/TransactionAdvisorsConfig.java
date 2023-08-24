package nl.rivm.screenit.main.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import org.springframework.aop.framework.autoproxy.DefaultAdvisorAutoProxyCreator;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.orm.hibernate5.HibernateTransactionManager;
import org.springframework.transaction.annotation.AnnotationTransactionAttributeSource;
import org.springframework.transaction.interceptor.TransactionAttributeSourceAdvisor;
import org.springframework.transaction.interceptor.TransactionInterceptor;

@Configuration
public class TransactionAdvisorsConfig
{

	@Bean
	public DefaultAdvisorAutoProxyCreator defaultAdvisorAutoProxyCreator()
	{
		return new DefaultAdvisorAutoProxyCreator();
	}

	@Bean
	public TransactionAttributeSourceAdvisor transactionAttributeSourceAdvisor(TransactionInterceptor transactionInterceptor)
	{
		TransactionAttributeSourceAdvisor bean = new TransactionAttributeSourceAdvisor();
		bean.setTransactionInterceptor(transactionInterceptor);
		return bean;
	}

	@Bean
	public TransactionInterceptor transactionInterceptor(HibernateTransactionManager transactionManager)
	{
		TransactionInterceptor bean = new TransactionInterceptor();
		bean.setTransactionManager(transactionManager);
		bean.setTransactionAttributeSource(new AnnotationTransactionAttributeSource());
		return bean;
	}
}
