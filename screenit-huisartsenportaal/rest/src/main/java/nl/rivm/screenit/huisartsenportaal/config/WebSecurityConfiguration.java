package nl.rivm.screenit.huisartsenportaal.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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

import nl.rivm.screenit.huisartsenportaal.config.autorizationproviders.ScreenITRegistrationAuthenticationProvider;
import nl.rivm.screenit.huisartsenportaal.config.autorizationproviders.ScreenITUsernamePasswordAuthenticationProvider;
import nl.rivm.screenit.huisartsenportaal.config.autorizationproviders.ScreenITWachtwoordVergetenAuthenticationProvider;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.header.writers.ReferrerPolicyHeaderWriter;

@Configuration
@EnableWebSecurity
public class WebSecurityConfiguration extends WebSecurityConfigurerAdapter
{
	@Autowired
	private ScreenITUsernamePasswordAuthenticationProvider usernamePasswordAuthentication;

	@Autowired
	private ScreenITRegistrationAuthenticationProvider registrationAuthenticationProvider;

	@Autowired
	private ScreenITWachtwoordVergetenAuthenticationProvider wachtwoordVergetenAuthenticationProvider;

	@Override
	protected void configure(AuthenticationManagerBuilder auth)
	{
		auth.authenticationProvider(wachtwoordVergetenAuthenticationProvider);
		auth.authenticationProvider(usernamePasswordAuthentication);
		auth.authenticationProvider(registrationAuthenticationProvider);
	}

	@Override
	@Bean
	public AuthenticationManager authenticationManagerBean() throws Exception
	{
		return super.authenticationManagerBean();
	}

	@Override
	protected void configure(HttpSecurity httpSecurity) throws Exception
	{
		httpSecurity
			.cors()
			.and()
			.csrf().disable()
			.authorizeRequests()
			.anyRequest()
			.permitAll()
			.and()
			.sessionManagement()
			.sessionCreationPolicy(SessionCreationPolicy.NEVER)
			.and()
			.headers()
			.contentSecurityPolicy("default-src 'self'; frame-src 'none'; frame-ancestors 'none'; upgrade-insecure-requests; object-src 'none';")
			.and()
			.referrerPolicy(ReferrerPolicyHeaderWriter.ReferrerPolicy.SAME_ORIGIN)
			.and()
			.contentTypeOptions();
	}
}
