package nl.rivm.screenit.wsb.config.cxf;

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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.ws.vrld.VragenlijstDefinitieService;
import nl.rivm.screenit.wsb.interceptors.BasicAuthAuthorizationInterceptor;
import nl.rivm.screenit.wsb.interceptors.LocationUriInterceptor;

import org.apache.cxf.Bus;
import org.apache.cxf.jaxws.EndpointImpl;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@AllArgsConstructor
public class VragenlijstDefinitieCxfConfig
{

	@Bean
	public EndpointImpl vragenlijstDefinitieEndpoint(Bus wsbBus, VragenlijstDefinitieService vragenlijstDefinitieService,
		BasicAuthAuthorizationInterceptor vragenlijstSecurityInterceptor, LocationUriInterceptor locationUriInterceptorVragenlijstDefinitie)
	{
		var endpoint = new EndpointImpl(wsbBus, vragenlijstDefinitieService);

		endpoint.getInInterceptors().addAll(List.of(vragenlijstSecurityInterceptor, locationUriInterceptorVragenlijstDefinitie));
		endpoint.publish("/vragenlijstdefinitieservice");
		return endpoint;
	}

	@Bean
	public LocationUriInterceptor locationUriInterceptorVragenlijstDefinitie()
	{
		var interceptor = new LocationUriInterceptor();
		interceptor.setWsContext("vragenlijstdefinitieservice");
		return interceptor;
	}

}
