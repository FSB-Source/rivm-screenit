package nl.rivm.screenit.main.web.openid;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.IOException;

import nl.rivm.screenit.main.service.impl.ScreenITOpenIdAssociationStoreImpl;
import nl.topicuszorg.spring.injection.SpringBeanProvider;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.openid4java.association.AssociationSessionType;
import org.openid4java.discovery.DiscoveryInformation;
import org.openid4java.discovery.yadis.YadisResolver;
import org.openid4java.message.Message;
import org.openid4java.message.ParameterList;
import org.openid4java.message.ax.AxMessage;
import org.openid4java.server.RealmVerifierFactory;
import org.openid4java.server.ServerManager;
import org.openid4java.util.HttpFetcherFactory;
import org.openid4java.util.HttpRequestOptions;

public class ScreenITOpenIdProviderHelper
{

	private static final Logger LOG = LoggerFactory.getLogger(ScreenITOpenIdProviderHelper.class);

	private static ServerManager serverManager;

	private static String openIdMount = "openid";

	private static String opEndpointUrl;

	private ScreenITOpenIdProviderHelper()
	{
	}

	public static ServerManager getServerManager()
	{
		LOG.trace("Haal Server Manager op");
		if (serverManager == null)
		{
			HttpRequestOptions options = HttpRequestOptions.getDefaultOptionsForDiscovery();
			options.addRequestHeader("Accept", "application/xrds+xml");
			options.setConnTimeout(60000);
			options.setCacheTTLSeconds(60000);
			serverManager = new ServerManager();
			serverManager.setOPEndpointUrl(ScreenITOpenIdProviderHelper.getOpEndpointUrl());
			serverManager.setPrivateAssociations(new ScreenITOpenIdAssociationStoreImpl());
			serverManager.setSharedAssociations(new ScreenITOpenIdAssociationStoreImpl());
			serverManager.setMinAssocSessEnc(AssociationSessionType.DH_SHA256);
			serverManager.setEnforceRpId(true);
			serverManager.setRealmVerifier(new RealmVerifierFactory(new YadisResolver(new HttpFetcherFactory().createFetcher(options))).getRealmVerifierForServer());
		}
		LOG.trace("Klaar met Server Manager");
		return serverManager;
	}

	public static Message verwerkAssociatieRequest(ServerManager manager, ParameterList request) throws IOException
	{
		LOG.trace("Start verwerking associatie verzoek");
		Message message = manager.associationResponse(request);
		LOG.trace("Klaar met verwerking associatie verzoek");
		return message;
	}

	public static Message verwerkVerificatieRequest(ServerManager manager, ParameterList request) throws IOException
	{
		LOG.trace("Start verwerking verificatie verzoek");
		Message message = manager.verify(request);
		LOG.trace("Klaar met verwerking verificatie verzoek");
		return message;
	}

	private static String maakXrdsDocument() throws IOException
	{
		LOG.trace("Start maken XRDS document");
		String endpoint = getOpEndpointUrl();
		XrdsDocumentBuilder documentBuilder = new XrdsDocumentBuilder();
		documentBuilder.addServiceElement(DiscoveryInformation.OPENID2_OP, endpoint, "10");
		documentBuilder.addServiceElement(DiscoveryInformation.OPENID2, endpoint, "20");
		documentBuilder.addServiceElement(AxMessage.OPENID_NS_AX, endpoint, "30");
		LOG.trace("Klaar met maken XRDS document");
		return documentBuilder.toXmlString();
	}

	public static String maakDiscoveryResponse() throws IOException
	{
		return maakXrdsDocument();
	}

	public static String getOpEndpointUrl()
	{
		if (opEndpointUrl != null)
		{
			return opEndpointUrl;
		}
		else
		{
			setOpEndpointUrl(maakOpenIdEndpointUrl());
			return opEndpointUrl;
		}
	}

	private static String maakOpenIdEndpointUrl()
	{
		String url = SpringBeanProvider.getInstance().getBean(String.class, "applicationUrl");
		if (!StringUtils.endsWith(url, "/"))
		{
			url += "/";
		}
		url += getOpenIdMount();
		return url;
	}

	private static void setOpEndpointUrl(String opEndpointUrl)
	{
		ScreenITOpenIdProviderHelper.opEndpointUrl = opEndpointUrl;
	}

	public static String getOpenIdMount()
	{
		return openIdMount;
	}
}
