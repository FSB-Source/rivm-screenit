package nl.rivm.screenit.mamma.planning.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import nl.rivm.screenit.mamma.planning.filter.PlanningControllerLoggingFilter;
import nl.rivm.screenit.mamma.planning.filter.PlanningControllerSynchronizedRequestFilter;

import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.orm.hibernate5.support.OpenSessionInViewFilter;

@Configuration
public class WebConfig
{

	@Bean
	public FilterRegistrationBean<PlanningControllerSynchronizedRequestFilter> synchronizedFilter()
	{
		FilterRegistrationBean<PlanningControllerSynchronizedRequestFilter> registrationBean
			= new FilterRegistrationBean<>();

		registrationBean.setFilter(new PlanningControllerSynchronizedRequestFilter());
		registrationBean.addUrlPatterns("/*");

		return registrationBean;
	}

	@Bean
	public FilterRegistrationBean<OpenSessionInViewFilter> openSessionInViewFilter()
	{
		FilterRegistrationBean<OpenSessionInViewFilter> registrationBean
			= new FilterRegistrationBean<>();

		registrationBean.setFilter(new OpenSessionInViewFilter());
		registrationBean.addInitParameter("sessionFactoryBeanName", "hibernateSessionFactory");
		registrationBean.addUrlPatterns("/*");

		return registrationBean;
	}

	@Bean
	public FilterRegistrationBean<PlanningControllerLoggingFilter> loggingFilter()
	{
		FilterRegistrationBean<PlanningControllerLoggingFilter> registrationBean
			= new FilterRegistrationBean<>();

		registrationBean.setFilter(new PlanningControllerLoggingFilter());
		registrationBean.addUrlPatterns("/*");

		return registrationBean;
	}

}
