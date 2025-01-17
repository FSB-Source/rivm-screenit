package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.File;
import java.security.Key;
import java.time.Instant;
import java.util.Collections;
import java.util.Date;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.inject.Inject;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.service.IdpSingleSignOnService;
import nl.rivm.screenit.main.service.KeyStoreService;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Configuration;

import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;

@Slf4j
@Configuration
public class IdpSingleSignOnServiceImpl implements IdpSingleSignOnService
{
	@Inject
	private SimplePreferenceService preferenceService;

	@Inject
	private KeyStoreService keyStoreService;

	@Inject
	@Qualifier("applicationEnvironment")
	private String applicationEnvironment;

	private Key key;

	@Override
	public String createWebFocusSsoUrl(InstellingGebruiker loggedInInstellingGebruiker)
	{
		Map<String, Object> context = createContextMap(loggedInInstellingGebruiker);

		return createJwtSsoUrl(loggedInInstellingGebruiker, context);
	}

	private String createJwtSsoUrl(InstellingGebruiker loggedInInstellingGebruiker, Map<String, Object> context)
	{
		Instant now = Instant.now();

		Gebruiker medewerker = loggedInInstellingGebruiker.getMedewerker();
		String jwt = Jwts.builder()
			.setIssuer(getIdpIssuer())
			.setIssuedAt(Date.from(now))
			.setNotBefore(Date.from(now))
			.setExpiration(Date.from(now.plusSeconds(getIdpExpiration()))) 
			.setSubject(medewerker.getId().toString())
			.claim("given_name", medewerker.getVoornaam())
			.claim("family_name", NaamUtil.getTussenvoegselEnAchternaam(medewerker))
			.claim("ctx", context)
			.claim("jti", UUID.randomUUID().toString())
			.claim("email_transient", medewerker.getEmailextra())
			.signWith(SignatureAlgorithm.RS512, getKey())
			.compact();

		LOG.trace("Token: " + jwt);

		return getIdpKeycloakOrigin() + "auth/realms/Professionals/sso/flows/" + getIdpFlowAlias() + "?client_id=" + getIdpClientId() + "&token=" + jwt;
	}

	private Key getKey()
	{
		if (key == null)
		{
			key = keyStoreService.getFirstKeyFromKeyStore(getIdpKeystoreLocation(), getIdpKeystorePassword(), "");
		}
		return key;
	}

	private Map<String, Object> createContextMap(InstellingGebruiker loggedInInstellingGebruiker)
	{
		return Collections.singletonMap("org",
			Stream.of(new Object[][] {
				{ "sub", loggedInInstellingGebruiker.getOrganisatie().getId() },
				{ "type", loggedInInstellingGebruiker.getOrganisatie().getOrganisatieType() },
			}).collect(Collectors.toMap(data -> (String) data[0], data -> data[1])));

	}

	private String getIdpKeystorePassword()
	{
		return getStringValue(PreferenceKey.INTERNAL_OPENID_CONNECT_IDP_KEYSTOREPASSWORD, "local");
	}

	private String getIdpKeystoreLocation()
	{
		String keystoreLocation = getStringValue(PreferenceKey.INTERNAL_OPENID_CONNECT_IDP_KEYSTORE, "");
		if (StringUtils.isBlank(keystoreLocation))
		{
			keystoreLocation = "keystore" + File.separator + "keycloak-screenit-webfocus-local.jks";
		}
		return keystoreLocation;
	}

	private String getIdpIssuer()
	{
		return getStringValue(PreferenceKey.INTERNAL_OPENID_CONNECT_IDP_ISSUER, "screenit-local");
	}

	private Integer getIdpExpiration()
	{
		return getIntegerValue(PreferenceKey.INTERNAL_OPENID_CONNECT_IDP_EXPIRATION, 10);
	}

	private String getIdpKeycloakOrigin()
	{
		String keycloakOrigin = getStringValue(PreferenceKey.INTERNAL_OPENID_CONNECT_IDP_KEYCLOAK_ORIGIN, "https://idp-acc.topicuszorg.nl/");
		if (!keycloakOrigin.endsWith("/"))
		{
			keycloakOrigin += "/";
		}
		return keycloakOrigin;
	}

	private String getIdpClientId()
	{
		return getStringValue(PreferenceKey.INTERNAL_OPENID_CONNECT_IDP_CLIENT_ID, "myaccount");
	}

	private String getIdpFlowAlias()
	{
		return getStringValue(PreferenceKey.INTERNAL_OPENID_CONNECT_IDP_FLOW_ALIAS, "sso-screenit");
	}

	private String getStringValue(PreferenceKey prefKey, String defaultValue)
	{
		if (useDbPreferences())
		{
			return preferenceService.getString(prefKey.name(), defaultValue);
		}
		return defaultValue;
	}

	private Integer getIntegerValue(PreferenceKey prefKey, Integer defaultValue)
	{
		if (useDbPreferences())
		{
			return preferenceService.getInteger(prefKey.name(), defaultValue);
		}
		return defaultValue;
	}

	private boolean useDbPreferences()
	{
		return !"Filler".equals(applicationEnvironment) && !"Unittest".equals(applicationEnvironment);
	}
}
