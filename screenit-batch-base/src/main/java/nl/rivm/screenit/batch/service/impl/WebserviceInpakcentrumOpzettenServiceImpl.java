package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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
import java.util.Map;

import javax.xml.ws.BindingProvider;

import nl.rivm.screenit.batch.service.WebserviceInpakcentrumOpzettenService;
import nl.rivm.screenit.util.logging.cxf.ScreenITLoggingInInterceptor;
import nl.rivm.screenit.util.logging.cxf.ScreenITLoggingOutInterceptor;
import nl.rivm.screenit.util.logging.cxf.ScreenITLoggingSaver;

import org.apache.cxf.configuration.jsse.TLSClientParameters;
import org.apache.cxf.endpoint.Client;
import org.apache.cxf.endpoint.ConduitSelector;
import org.apache.cxf.endpoint.PreexistingConduitSelector;
import org.apache.cxf.frontend.ClientProxy;
import org.apache.cxf.transport.http.HTTPConduit;
import org.apache.cxf.ws.policy.PolicyConstants;
import org.apache.neethi.Policy;
import org.apache.neethi.PolicyComponent;
import org.springframework.beans.factory.annotation.Autowired;
import org.tempuri.DaklapackWebService;
import org.tempuri.IUpload;

public class WebserviceInpakcentrumOpzettenServiceImpl implements WebserviceInpakcentrumOpzettenService
{
	@Autowired
	private ScreenITLoggingSaver loggingSaver;

	@Autowired
	private String inpakCentrumEndpointUrl;

	@Autowired
	private Boolean testModus;

	@Override
	public IUpload initialiseerWebserviceInpakcentrum()
	{
		DaklapackWebService webserviceClient = new DaklapackWebService();
		IUpload upload = webserviceClient.getDataUploadEndpoint();
		BindingProvider bindingProvider = (BindingProvider) upload;
		Map<String, Object> requestContext = bindingProvider.getRequestContext();
		requestContext.put(BindingProvider.ENDPOINT_ADDRESS_PROPERTY, inpakCentrumEndpointUrl);

		if (Boolean.TRUE.equals(testModus) && inpakCentrumEndpointUrl.startsWith("http:"))
		{
			requestContext.put(PolicyConstants.POLICY_OVERRIDE, new HttpPolicy());
		}

		Client client = ClientProxy.getClient(upload);
		client.getInInterceptors().add(new ScreenITLoggingInInterceptor(loggingSaver));
		client.getOutInterceptors().add(new ScreenITLoggingOutInterceptor(loggingSaver));

		HTTPConduit httpConduit = (HTTPConduit) client.getConduit();

		TLSClientParameters tlsClientParameters = new TLSClientParameters();
		tlsClientParameters.setDisableCNCheck(true);

		httpConduit.setTlsClientParameters(tlsClientParameters);

		ConduitSelector selector = new PreexistingConduitSelector(httpConduit, client.getEndpoint());
		client.setConduitSelector(selector);

		return upload;
	}

	private class HttpPolicy extends Policy
	{
		@Override
		public void addPolicyComponent(PolicyComponent component)
		{

		}

		@Override
		public void addPolicyComponents(List<? extends PolicyComponent> components)
		{

		}
	}
}
