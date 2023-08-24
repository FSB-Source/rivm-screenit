package nl.rivm.screenit.clientportaal;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.service.DistributedLockService;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5Session;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5SessionInThread;
import nl.topicuszorg.patientregistratie.persoonsgegevens.service.BsnService;

import org.apache.catalina.Container;
import org.apache.catalina.core.StandardHost;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.websocket.servlet.TomcatWebSocketServletWebServerCustomizer;
import org.springframework.boot.web.embedded.tomcat.TomcatServletWebServerFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.FilterType;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

@Slf4j
@EnableJpaRepositories(basePackages = { "nl.rivm.screenit" })
@ComponentScan(
	basePackages = { "nl.rivm.screenit", "nl.topicuszorg" },
	excludeFilters = {
		@ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE, value = DistributedLockService.class),
		@ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE, value = BsnService.class),
		@ComponentScan.Filter(type = FilterType.REGEX, pattern = "nl.topicuszorg.hibernate.spring.module.test.impl.TestServiceImpl"),
		@ComponentScan.Filter(type = FilterType.REGEX, pattern = "nl.topicuszorg.loginformatie.services.impl.LogInformatieServiceImpl"),
		@ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE, value = OpenHibernate5SessionInThread.class),
		@ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE, value = OpenHibernate5Session.class),
		@ComponentScan.Filter(type = FilterType.REGEX, pattern = "nl.topicuszorg.hl7.*")
	})
@SpringBootApplication
public class Application
{

	public static void main(String[] args)
	{
		SpringApplication.run(Application.class, args);
	}

	@Bean
	public TomcatWebSocketServletWebServerCustomizer errorValveCustomizer()
	{
		return new TomcatWebSocketServletWebServerCustomizer()
		{
			@Override
			public void customize(TomcatServletWebServerFactory factory)
			{
				factory.addContextCustomizers((context) ->
				{
					Container parent = context.getParent();
					if (parent instanceof StandardHost)
					{
						StandardHost standardHost = (StandardHost) parent;
						standardHost.setErrorReportValveClass("nl.rivm.screenit.clientportaal.filter.CustomTomcatErrorValve");
					}
				});
			}

			@Override
			public int getOrder()
			{
				return 100; 
			}
		};
	}
}
