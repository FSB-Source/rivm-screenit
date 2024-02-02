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

import java.util.List;

import nl.rivm.screenit.wsb.fhir.servlet.dstu3.v1.RestfulServlet;

import org.apache.cxf.transport.servlet.CXFServlet;
import org.springframework.boot.web.servlet.ServletRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class ServletConfig
{

	@Bean
	public ServletRegistrationBean<CXFServlet> cxfServlet()
	{
		var servlet = new ServletRegistrationBean<CXFServlet>();
		servlet.setServlet(new CXFServlet());
		servlet.setLoadOnStartup(1);
		servlet.addInitParameter("hide-service-list-page", "true");
		servlet.setUrlMappings(List.of("/services/*"));
		return servlet;
	}

	@Bean
	public ServletRegistrationBean<RestfulServlet> restfulServlet()
	{
		var servlet = new ServletRegistrationBean<RestfulServlet>();
		servlet.setServlet(new RestfulServlet());
		servlet.setLoadOnStartup(1);
		servlet.setUrlMappings(List.of("/services/rest/bmhk/huisarts/fhir/dstu3/v1/*"));
		return servlet;
	}

}
