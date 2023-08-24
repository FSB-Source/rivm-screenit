package nl.rivm.screenit.wsb.config.cxf;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.ws.providedocument.ProvideDocumentPortType;
import nl.rivm.screenit.wsb.interceptors.CertificaatInterceptor;
import nl.rivm.screenit.wsb.interceptors.LocationUriInterceptor;
import nl.rivm.screenit.wsb.pd.interceptor.SchematronInterceptor;

import org.apache.cxf.Bus;
import org.apache.cxf.binding.soap.Soap12;
import org.apache.cxf.binding.soap.SoapBindingConfiguration;
import org.apache.cxf.jaxws.EndpointImpl;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@AllArgsConstructor
public class ProvideDocumentCxfConfig
{

	@Bean
	public EndpointImpl provideDocumentEndpoint(Bus wsbBus, ProvideDocumentPortType provideDocumentPortType,
		CertificaatInterceptor certificaatInterceptor,
		SchematronInterceptor schematronInterceptor, LocationUriInterceptor locationUriInterceptorVerslagen)
	{
		var endpoint = new EndpointImpl(wsbBus, provideDocumentPortType);

		var bindingConfig = new SoapBindingConfiguration();
		bindingConfig.setMtomEnabled(false);
		bindingConfig.setVersion(Soap12.getInstance());

		endpoint.getInInterceptors().addAll(List.of(certificaatInterceptor, schematronInterceptor, locationUriInterceptorVerslagen));
		endpoint.getProperties().put("mtom-enabled", false);
		endpoint.getProperties().put("org.apache.cxf.stax.force-start-document", true);
		endpoint.getProperties().put("faultStackTraceEnabled", true);
		endpoint.setBindingConfig(bindingConfig);
		endpoint.publish("/verslagen");
		return endpoint;
	}

	@Bean
	public CertificaatInterceptor certificaatInterceptor(Boolean fqdncontrole)
	{
		var interceptor = new CertificaatInterceptor();
		interceptor.setFqdncontrole(fqdncontrole);
		return interceptor;
	}

	@Bean
	public LocationUriInterceptor locationUriInterceptorVerslagen()
	{
		var interceptor = new LocationUriInterceptor();
		interceptor.setWsContext("verslagen");
		return interceptor;
	}

}
