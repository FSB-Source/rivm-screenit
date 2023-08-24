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

import nl.rivm.screenit.mamma.se.security.SERealm;

import org.apache.shiro.cache.ehcache.EhCacheManager;
import org.apache.shiro.mgt.SecurityManager;
import org.apache.shiro.spring.web.ShiroFilterFactoryBean;
import org.apache.shiro.web.mgt.DefaultWebSecurityManager;
import org.apache.shiro.web.servlet.AbstractShiroFilter;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.Ordered;

@Configuration
public class ShiroConfig
{

	@Bean
	public SecurityManager shiroSecurityManager(SERealm seRealm, EhCacheManager cacheManager)
	{
		var securityManager = new DefaultWebSecurityManager();
		securityManager.setRealm(seRealm);
		securityManager.setCacheManager(cacheManager);
		return securityManager;
	}

	@Bean
	public ShiroFilterFactoryBean shiroFilter(SecurityManager shiroSecurityManager)
	{
		var filter = new ShiroFilterFactoryBean();
		filter.setSecurityManager(shiroSecurityManager);
		return filter;
	}

	@Bean
	public FilterRegistrationBean<AbstractShiroFilter> shiroFilterProxy(ShiroFilterFactoryBean shiroFilter) throws Exception
	{
		var filter = new FilterRegistrationBean<AbstractShiroFilter>();
		filter.setFilter(shiroFilter.getObject());
		filter.setName("shiroFilter");
		filter.addInitParameter("targetFilterLifecycle", "true");
		filter.addUrlPatterns("/*");
		filter.setOrder(Ordered.HIGHEST_PRECEDENCE);
		return filter;
	}

}
