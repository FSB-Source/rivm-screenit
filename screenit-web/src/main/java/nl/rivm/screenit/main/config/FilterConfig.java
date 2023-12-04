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

import java.util.List;

import nl.rivm.screenit.main.filter.AuthorizationFilter;
import nl.rivm.screenit.main.web.ScreenitSessionListener;
import nl.rivm.screenit.main.web.filter.LogFilter;
import nl.rivm.screenit.main.web.filter.PlanningRestErrorHandlerFilter;
import nl.rivm.screenit.main.web.filter.SecurityHeadersFilter;
import nl.rivm.screenit.main.web.status.StatusServlet;
import nl.topicuszorg.wicket.filter.CacheFilter;

import org.apache.wicket.protocol.http.WicketFilter;
import org.apache.wicket.protocol.http.servlet.WicketSessionFilter;
import org.apache.wicket.spring.SpringWebApplicationFactory;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.boot.web.servlet.ServletListenerRegistrationBean;
import org.springframework.boot.web.servlet.ServletRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.orm.hibernate5.support.OpenSessionInViewFilter;

@Configuration
public class FilterConfig
{

	private static final String WICKET_FILTER_NAME = "wicketFilter";

	private enum FilterOrder
	{
		SECURITY_HEADERS,
		CACHE,
		PLANNING_REST_ERROR_HANDLER,
		OPEN_SESSION_IN_VIEW,
		LOG,
		WICKET,
		WICKET_SESSION,
		AUTHORIZATION,
	}

	@Bean
	public ServletListenerRegistrationBean<ScreenitSessionListener> screenitSessionListener()
	{
		var listener = new ServletListenerRegistrationBean<ScreenitSessionListener>();
		listener.setListener(new ScreenitSessionListener());
		return listener;
	}

	@Bean
	public ServletRegistrationBean<StatusServlet> statusServlet()
	{
		var servlet = new ServletRegistrationBean<StatusServlet>();
		servlet.setServlet(new StatusServlet());
		servlet.setUrlMappings(List.of("/status/*"));
		return servlet;
	}

	@Bean
	public FilterRegistrationBean<WicketFilter> wicketFilter()
	{
		var filter = new FilterRegistrationBean<WicketFilter>();
		filter.setFilter(new WicketFilter());
		filter.setName(WICKET_FILTER_NAME);
		filter.addInitParameter(WicketFilter.APP_FACT_PARAM, SpringWebApplicationFactory.class.getName());
		filter.addInitParameter(WicketFilter.FILTER_MAPPING_PARAM, "/*");
		filter.addInitParameter(WicketFilter.IGNORE_PATHS_PARAM, "api,rest,assets");
		filter.addUrlPatterns("/*");
		filter.setOrder(FilterOrder.WICKET.ordinal());
		return filter;
	}

	@Bean
	public FilterRegistrationBean<WicketSessionFilter> wicketSessionFilter()
	{
		var filter = new FilterRegistrationBean<WicketSessionFilter>();
		filter.setFilter(new WicketSessionFilter());
		filter.addInitParameter("filterName", WICKET_FILTER_NAME);
		filter.addUrlPatterns("/api/*");
		filter.setOrder(FilterOrder.WICKET_SESSION.ordinal());
		return filter;
	}

	@Bean
	public FilterRegistrationBean<SecurityHeadersFilter> securityHeadersFilter()
	{
		var filter = new FilterRegistrationBean<SecurityHeadersFilter>();
		filter.setFilter(new SecurityHeadersFilter());
		filter.addUrlPatterns("/*");
		filter.setOrder(FilterOrder.SECURITY_HEADERS.ordinal());
		return filter;
	}

	@Bean
	public FilterRegistrationBean<CacheFilter> cacheFilter()
	{
		var filter = new FilterRegistrationBean<CacheFilter>();
		filter.setFilter(new CacheFilter());
		filter.addInitParameter(WicketFilter.FILTER_MAPPING_PARAM, "/*");
		filter.addUrlPatterns("/*");
		filter.setOrder(FilterOrder.CACHE.ordinal());
		return filter;
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
	public FilterRegistrationBean<LogFilter> logFilter()
	{
		var filter = new FilterRegistrationBean<LogFilter>();
		filter.setFilter(new LogFilter());
		filter.addInitParameter("sessionFactoryBeanName", "hibernateSessionFactory");
		filter.addUrlPatterns("/*");
		filter.setOrder(FilterOrder.LOG.ordinal());
		return filter;
	}

	@Bean
	public FilterRegistrationBean<PlanningRestErrorHandlerFilter> planningRestErrorHandlerFilter()
	{
		var filter = new FilterRegistrationBean<PlanningRestErrorHandlerFilter>();
		filter.setFilter(new PlanningRestErrorHandlerFilter());
		filter.addInitParameter(WicketFilter.FILTER_MAPPING_PARAM, "/*");
		filter.addUrlPatterns("/*");
		filter.setOrder(FilterOrder.PLANNING_REST_ERROR_HANDLER.ordinal());
		return filter;
	}

	@Bean
	public FilterRegistrationBean<AuthorizationFilter> authorizationFilter()
	{
		var filter = new FilterRegistrationBean<AuthorizationFilter>();
		filter.setFilter(new AuthorizationFilter());
		filter.addUrlPatterns("/api/*");
		filter.setOrder(FilterOrder.AUTHORIZATION.ordinal());
		return filter;
	}
}
