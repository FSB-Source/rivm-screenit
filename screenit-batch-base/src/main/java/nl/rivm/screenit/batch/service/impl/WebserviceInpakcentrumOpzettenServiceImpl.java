package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.batch.service.WebserviceInpakcentrumOpzettenService;
import nl.rivm.screenit.batch.util.LoggingOutInterceptor;
import org.apache.commons.lang.StringUtils;
import org.apache.cxf.configuration.jsse.TLSClientParameters;
import org.apache.cxf.endpoint.Client;
import org.apache.cxf.endpoint.ConduitSelector;
import org.apache.cxf.endpoint.PreexistingConduitSelector;
import org.apache.cxf.frontend.ClientProxy;
import org.apache.cxf.interceptor.LoggingInInterceptor;
import org.apache.cxf.transport.http.HTTPConduit;
import org.apache.cxf.transports.http.configuration.ProxyServerType;
import org.apache.cxf.ws.policy.PolicyConstants;
import org.apache.neethi.Policy;
import org.apache.neethi.PolicyComponent;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.tempuri.DaklapackWebService;
import org.tempuri.IUpload;

import javax.xml.ws.BindingProvider;
import java.util.List;
import java.util.Map;

@Service
public class WebserviceInpakcentrumOpzettenServiceImpl implements WebserviceInpakcentrumOpzettenService
{
    @Autowired
    @Qualifier(value = "inpakCentrumEndpointUrl")
    private String inpakCentrumEndpointUrl;

    @Autowired
    @Qualifier(value = "inpakCentrumProxyServer")
    private String inpakCentrumProxyServer;

    @Autowired
    @Qualifier(value = "inpakCentrumProxyServerPort")
    private Integer inpakCentrumProxyServerPort;

    @Autowired
    @Qualifier(value = "testModus")
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
        client.getInInterceptors().add(new LoggingInInterceptor());
        client.getOutInterceptors().add(new LoggingOutInterceptor());

        HTTPConduit httpConduit = (HTTPConduit) client.getConduit();

        TLSClientParameters tlsClientParameters = new TLSClientParameters();
        tlsClientParameters.setDisableCNCheck(true);

        httpConduit.setTlsClientParameters(tlsClientParameters);

        if (StringUtils.isNotBlank(inpakCentrumProxyServer))
        {
            httpConduit.getClient().setProxyServerType(ProxyServerType.HTTP);
            httpConduit.getClient().setProxyServer(inpakCentrumProxyServer);
        }

        if (inpakCentrumProxyServerPort != null && inpakCentrumProxyServerPort > 0)
        {
            httpConduit.getClient().setProxyServerPort(inpakCentrumProxyServerPort);
        }

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
