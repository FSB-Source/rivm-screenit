package nl.rivm.screenit.mamma.se.proxy.configuration;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.web.header.writers.ReferrerPolicyHeaderWriter;
import org.springframework.security.web.header.writers.StaticHeadersWriter;

@Configuration
public class SecurityConfig extends WebSecurityConfigurerAdapter
{

	private static final String NFC_ENDPOINT = "http://localhost:5001";

	private static final String IMS_ENDPOINT = "https://localhost:7001";

	@Autowired
	public void configureGlobal(AuthenticationManagerBuilder auth) throws Exception
	{
		auth
			.inMemoryAuthentication();
	}

	@Override
	protected void configure(HttpSecurity http) throws Exception
	{
		String contentSecurityPolicy = String.join(" ; ", new String[] {
			createSecurityRule("default-src", "'self'"),
			createSecurityRule("style-src", "'self'", "'unsafe-inline'"),
			createSecurityRule("script-src", "'self'", "'unsafe-inline'"),
			createSecurityRule("img-src", "'self'", "data:"),
			createSecurityRule("connect-src",
				"'self'",
				NFC_ENDPOINT,
				IMS_ENDPOINT,
				"ws:", "wss:")
		});
		http
			.csrf().disable()
			.cors().disable()
			.headers().referrerPolicy(ReferrerPolicyHeaderWriter.ReferrerPolicy.SAME_ORIGIN).and()
			.contentTypeOptions().and()
			.cacheControl().and()
			.frameOptions().sameOrigin()
			.httpStrictTransportSecurity().and()
			.addHeaderWriter(new StaticHeadersWriter("Content-Security-Policy", contentSecurityPolicy));
	}

	private String createSecurityRule(String srcType, String... allowedSources)
	{
		return srcType + " " + String.join(" ", allowedSources);
	}
}
