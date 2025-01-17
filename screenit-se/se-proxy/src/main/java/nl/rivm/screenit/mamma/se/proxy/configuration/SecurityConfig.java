package nl.rivm.screenit.mamma.se.proxy.configuration;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.header.writers.ReferrerPolicyHeaderWriter;
import org.springframework.security.web.header.writers.StaticHeadersWriter;

import static org.springframework.security.config.Customizer.withDefaults;

@Configuration
@EnableWebSecurity
public class SecurityConfig
{
	private static final String NFC_ENDPOINT = "http://localhost:5001";

	private static final String IMS_ENDPOINT = "https://localhost:7001";

	@Bean
	@SuppressWarnings(

		"java:S4502"
	)
	SecurityFilterChain filterChain(HttpSecurity http) throws Exception
	{
		http.csrf(AbstractHttpConfigurer::disable)
			.cors(AbstractHttpConfigurer::disable)
			.headers(headers -> headers.referrerPolicy(
					policy -> policy.policy(ReferrerPolicyHeaderWriter.ReferrerPolicy.SAME_ORIGIN))
				.contentTypeOptions(withDefaults())
				.cacheControl(withDefaults())
				.frameOptions(options -> options.sameOrigin()
					.httpStrictTransportSecurity(withDefaults()))
				.addHeaderWriter(new StaticHeadersWriter("Content-Security-Policy", getContentSecurityPolicy())));

		return http.build();
	}

	@Autowired
	public void configureGlobal(AuthenticationManagerBuilder auth) throws Exception
	{
		auth.inMemoryAuthentication();
	}

	private static String getContentSecurityPolicy()
	{
		return String.join(" ; ",
			createCspRule("default-src", "'self'"),
			createCspRule("style-src", "'self'", "'unsafe-inline'"),
			createCspRule("script-src", "'self'", "'unsafe-inline'"),
			createCspRule("img-src", "'self'", "data:"),
			createCspRule("connect-src", "'self'", NFC_ENDPOINT, IMS_ENDPOINT, "ws:", "wss:")
		);
	}

	private static String createCspRule(String srcType, String... allowedSources)
	{
		return srcType + " " + String.join(" ", allowedSources);
	}
}
