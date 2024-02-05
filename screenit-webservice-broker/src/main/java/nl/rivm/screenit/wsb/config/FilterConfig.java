package nl.rivm.screenit.wsb.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
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

import nl.rivm.screenit.wsb.filter.WsbControllerSynchronizedRequestFilter;
import nl.rivm.screenit.wsb.filter.WsbRestControllerLoggingFilter;

import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.orm.hibernate5.support.OpenSessionInViewFilter;

@Configuration
public class FilterConfig
{
	private enum FilterOrder
	{
		SYNCHRONIZED,
		OPEN_SESSION_IN_VIEW,
		LOG,
	}

	@Bean
	public FilterRegistrationBean<OpenSessionInViewFilter> openSessionInViewFilter()
	{
		var filter = new FilterRegistrationBean<OpenSessionInViewFilter>();
		filter.setFilter(new OpenSessionInViewFilter());
		filter.addInitParameter("sessionFactoryBeanName", "hibernateSessionFactory");
		filter.addUrlPatterns("/*");
		filter.setOrder(FilterOrder.OPEN_SESSION_IN_VIEW.ordinal());
		return filter;
	}

	@Bean
	public FilterRegistrationBean<WsbControllerSynchronizedRequestFilter> synchronizedFilter()
	{
		var filter = new FilterRegistrationBean<WsbControllerSynchronizedRequestFilter>();
		filter.setFilter(new WsbControllerSynchronizedRequestFilter());
		filter.addUrlPatterns("/*");
		filter.setOrder(FilterOrder.SYNCHRONIZED.ordinal());
		return filter;
	}

	@Bean
	public FilterRegistrationBean<WsbRestControllerLoggingFilter> logFilter(WsbRestControllerLoggingFilter restControllerLoggingFilter)
	{
		var filter = new FilterRegistrationBean<WsbRestControllerLoggingFilter>();
		filter.setFilter(restControllerLoggingFilter);
		filter.addUrlPatterns("/services/rest/*");
		filter.setOrder(FilterOrder.LOG.ordinal());
		return filter;
	}

}
