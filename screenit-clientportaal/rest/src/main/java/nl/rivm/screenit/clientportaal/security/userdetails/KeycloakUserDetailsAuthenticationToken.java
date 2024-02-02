package nl.rivm.screenit.clientportaal.security.userdetails;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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

import java.util.Collection;

import org.keycloak.adapters.OidcKeycloakAccount;
import org.keycloak.adapters.springsecurity.token.KeycloakAuthenticationToken;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.util.Assert;

public class KeycloakUserDetailsAuthenticationToken extends KeycloakAuthenticationToken
{

	private final UserDetails userDetails;

	public KeycloakUserDetailsAuthenticationToken(UserDetails userDetails, OidcKeycloakAccount account,
		Collection<? extends GrantedAuthority> authorities)
	{
		super(account, false, authorities);
		Assert.notNull(userDetails, "UserDetails required");
		this.userDetails = userDetails;
	}

	@Override
	public Object getPrincipal()
	{
		return userDetails;
	}

}
