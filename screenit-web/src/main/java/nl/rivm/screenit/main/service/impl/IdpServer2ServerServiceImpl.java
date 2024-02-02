package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.File;
import java.security.Key;

import javax.inject.Inject;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.service.IdpServer2ServerService;
import nl.rivm.screenit.main.service.KeyStoreService;
import nl.rivm.screenit.util.EnvironmentUtil;
import nl.topicuszorg.idp.client.credentials.IdpClient;
import nl.topicuszorg.idp.client.credentials.IdpClientCredentialsService;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.ApplicationListener;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.event.ContextRefreshedEvent;

@Slf4j
@Configuration
public class IdpServer2ServerServiceImpl implements IdpServer2ServerService, ApplicationListener<ContextRefreshedEvent>
{
	@Inject
	private KeyStoreService keyStoreService;

	@Inject
	@Qualifier("applicationEnvironment")
	private String applicationEnvironment;

	private boolean doInit = true;

	private IdpClientCredentialsService idpClientCredentialsService;

	private IdpClient idpClientZorgId;

	@Override
	public void onApplicationEvent(ContextRefreshedEvent contextRefreshedEvent)
	{
		if (doInit && !"Filler".equals(applicationEnvironment))
		{
			LOG.info("doInit");
			try
			{
				idpClientCredentialsService = new IdpClientCredentialsService();
				idpClientZorgId = maakIdpClientVoorZorgId();
				LOG.info("IdpClient voor ZorgId aangemaakt");
			}
			catch (Exception e)
			{
				LOG.error("Fout bij opstarten", e);
			}
			finally
			{
				doInit = false;
			}
		}
	}

	private IdpClient maakIdpClientVoorZorgId()
	{
		var audience = idpAudience();
		var idpTokenEndpoint = audience + "/protocol/openid-connect/token";
		return new IdpClient(audience, idpClientId(), signingKey(), clientAssertionLifespan(), idpTokenEndpoint, expirationOffsetSeconds(), zorgIdScope());
	}

	private Key signingKey()
	{
		return keyStoreService.getFirstKeyFromKeyStore(keystoreLocationOnFilestore(), keyStorePassword(), keyStorePassword());
	}

	@Override
	public String getIdpAccessTokenVoorZorgId()
	{
		try
		{
			return idpClientCredentialsService.getAccessToken(idpClientZorgId);
		}
		catch (Exception e)
		{
			LOG.error("Fout bij het opvragen IdP token voor idp token endpoint '{}': {}", idpClientZorgId.getIdpTokenEndpoint(), e);
			return null;
		}
	}

	private String idpAudience()
	{
		return EnvironmentUtil.getStringEnvironmentVariable("IDP_S2S_AUDIENCE", "https://test.login.topicuszorg.nl/auth/realms/Apps");
	}

	private String idpClientId()
	{
		return EnvironmentUtil.getStringEnvironmentVariable("IDP_S2S_CLIENTID", "screenit-client-credentials"); 
	}

	private int clientAssertionLifespan()
	{

		return EnvironmentUtil.getIntegerEnvironmentVariable("IDP_S2S_CLIENT_ASSERTION_LIFESPAN", 10);
	}

	private int expirationOffsetSeconds()
	{

		return EnvironmentUtil.getIntegerEnvironmentVariable("IDP_S2S_EXPIRATION_OFFSET_SECONDS", 10);
	}

	private String zorgIdScope()
	{
		return EnvironmentUtil.getStringEnvironmentVariable("IDP_S2S_ZORGID_SCOPE", "zorg-id"); 
	}

	private String keystoreLocationOnFilestore()
	{
		return EnvironmentUtil.getStringEnvironmentVariable("IDP_S2S_KEYSTORE_LOCATION", "keystore" + File.separator + "screenit-client-credentials-test.jks");
	}

	private String keyStorePassword()
	{

		return EnvironmentUtil.getStringEnvironmentVariable("IDP_S2S_KEYSTORE_PASSWORD", "Dc7GsIHO7Ix7Q0tx2AAFjBejOl");
	}
}
