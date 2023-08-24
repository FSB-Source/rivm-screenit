package nl.rivm.screenit.mamma.se.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import nl.rivm.screenit.mamma.se.filter.SELogFilter;
import nl.rivm.screenit.mamma.se.filter.SEResetAccountRequestFilter;
import nl.rivm.screenit.mamma.se.filter.SeRequestLimitFilter;

import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.orm.hibernate5.support.OpenSessionInViewFilter;

import com.thetransactioncompany.cors.CORSFilter;

@Configuration
public class FilterConfig
{
	private enum FilterOrder
	{
		CORS,
		REQUEST_LIMIT,
		OPEN_SESSION_IN_VIEW,
		RESET_ACCOUNT_REQUEST,
		LOG,
	}

	@Bean
	public FilterRegistrationBean<OpenSessionInViewFilter> openSessionInViewFilter()
	{
		var filter = new FilterRegistrationBean<OpenSessionInViewFilter>();
		filter.setFilter(new OpenSessionInViewFilter());
		filter.addInitParameter("sessionFactoryBeanName", "hibernateSessionFactory");
		filter.addUrlPatterns("/api/*");
		filter.setOrder(FilterOrder.OPEN_SESSION_IN_VIEW.ordinal());
		return filter;
	}

	@Bean
	public FilterRegistrationBean<CORSFilter> corsFilter()
	{
		var filter = new FilterRegistrationBean<CORSFilter>();
		filter.setFilter(new CORSFilter());
		filter.addInitParameter("cors.supportedHeaders", "Content-Type,Accept,Origin");
		filter.addUrlPatterns("/api/*");
		filter.setOrder(FilterOrder.CORS.ordinal());
		return filter;
	}

	@Bean
	public FilterRegistrationBean<SELogFilter> logFilter()
	{
		var filter = new FilterRegistrationBean<SELogFilter>();
		filter.setFilter(new SELogFilter());
		filter.addInitParameter("sessionFactoryBeanName", "hibernateSessionFactory");
		filter.addUrlPatterns("/api/*");
		filter.setOrder(FilterOrder.LOG.ordinal());
		return filter;
	}

	@Bean
	public FilterRegistrationBean<SeRequestLimitFilter> requestLimitFilter()
	{
		var filter = new FilterRegistrationBean<SeRequestLimitFilter>();
		filter.setFilter(new SeRequestLimitFilter());
		filter.addInitParameter("sessionFactoryBeanName", "hibernateSessionFactory");
		filter.addUrlPatterns("/api/*");
		filter.setOrder(FilterOrder.REQUEST_LIMIT.ordinal());
		return filter;
	}

	@Bean
	public FilterRegistrationBean<SEResetAccountRequestFilter> resetAccountRequest()
	{
		var filter = new FilterRegistrationBean<SEResetAccountRequestFilter>();
		filter.setFilter(new SEResetAccountRequestFilter());
		filter.addUrlPatterns("/api/*");
		filter.setOrder(FilterOrder.RESET_ACCOUNT_REQUEST.ordinal());
		return filter;
	}

}
