package nl.rivm.screenit.clientportaal.security;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.security.Principal;

import nl.rivm.screenit.clientportaal.security.userdetails.KeycloakUserDetailsAuthenticationToken;
import nl.rivm.screenit.clientportaal.security.userdetails.ScreenitUserDetails;
import nl.rivm.screenit.clientportaal.security.userdetails.ScreenitUserDetailsService;

import org.keycloak.adapters.OidcKeycloakAccount;
import org.keycloak.adapters.springsecurity.authentication.KeycloakAuthenticationProvider;
import org.keycloak.adapters.springsecurity.token.KeycloakAuthenticationToken;
import org.springframework.beans.factory.annotation.Required;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.util.Assert;

public class ScreenItKeycloakAuthenticationProvider extends KeycloakAuthenticationProvider
{

	private ScreenitUserDetailsService userDetailsService;

	@Override
	public Authentication authenticate(Authentication authentication) throws AuthenticationException
	{
		KeycloakAuthenticationToken token = (KeycloakAuthenticationToken) super.authenticate(authentication);
		String username;
		ScreenitUserDetails userDetails;

		if (token == null)
		{
			return null;
		}

		username = this.resolveUsername(token);
		userDetails = userDetailsService.loadUserByUsername(username.replace("screenit-test-user-", ""));

		return new KeycloakUserDetailsAuthenticationToken(userDetails, token.getAccount(), token.getAuthorities());
	}

	protected String resolveUsername(KeycloakAuthenticationToken token)
	{
		Assert.notNull(token, "KeycloakAuthenticationToken required");
		Assert.notNull(token.getAccount(), "KeycloakAuthenticationToken.getAccount() cannot be return null");
		OidcKeycloakAccount account = token.getAccount();
		Principal principal = account.getPrincipal();

		return principal.getName();
	}

	@Required
	public void setUserDetailsService(ScreenitUserDetailsService userDetailsService)
	{
		this.userDetailsService = userDetailsService;
	}
}
