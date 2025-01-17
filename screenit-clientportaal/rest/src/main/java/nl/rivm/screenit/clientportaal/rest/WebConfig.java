package nl.rivm.screenit.clientportaal.rest;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.clientportaal.rest.mapping.CustomObjectMapper;

import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.orm.hibernate5.support.OpenSessionInViewFilter;
import org.springframework.web.servlet.config.annotation.ContentNegotiationConfigurer;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.servlet.config.annotation.PathMatchConfigurer;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@EnableWebMvc
@AllArgsConstructor
@Configuration
public class WebConfig implements WebMvcConfigurer
{

	private final CustomObjectMapper customObjectMapper;

	@Bean
	public FilterRegistrationBean<OpenSessionInViewFilter> openSessionInViewFilter()
	{
		var filter = new FilterRegistrationBean<OpenSessionInViewFilter>();
		filter.setFilter(new OpenSessionInViewFilter());
		filter.addInitParameter("sessionFactoryBeanName", "hibernateSessionFactory");
		filter.addUrlPatterns("/*");
		return filter;
	}

	@Override
	public void configureMessageConverters(List<HttpMessageConverter<?>> converters)
	{
		MappingJackson2HttpMessageConverter jacksonConverter = new MappingJackson2HttpMessageConverter();
		jacksonConverter.setObjectMapper(customObjectMapper);
		converters.add(jacksonConverter);
	}

	@Override
	public void configureContentNegotiation(ContentNegotiationConfigurer configurer)
	{

		configurer.favorPathExtension(false);
	}

	@Override
	public void configurePathMatch(PathMatchConfigurer matcher)
	{

		matcher.setUseSuffixPatternMatch(false);
	}
}
