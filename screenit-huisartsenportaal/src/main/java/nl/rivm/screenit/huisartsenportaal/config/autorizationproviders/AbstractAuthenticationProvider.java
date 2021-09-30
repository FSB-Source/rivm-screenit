package nl.rivm.screenit.huisartsenportaal.config.autorizationproviders;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2016 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Map;

import nl.rivm.screenit.huisartsenportaal.model.enums.Scope;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.authentication.dao.AbstractUserDetailsAuthenticationProvider;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UsernameNotFoundException;

public abstract class AbstractAuthenticationProvider extends AbstractUserDetailsAuthenticationProvider
{

	private static final Logger LOG = LoggerFactory.getLogger(AbstractAuthenticationProvider.class);

	private static String SCOPEKEY = "scope";

	private String credentials;

	@Override
	protected UserDetails retrieveUser(String username, UsernamePasswordAuthenticationToken authentication) throws AuthenticationException
	{
		UserDetails user = null;
		Map<String, String> parameters = (Map<String, String>) authentication.getDetails();
		if (parameters.containsKey(SCOPEKEY))
		{
			String requestScope = parameters.get(SCOPEKEY);
			if (getScope().equals(requestScope))
			{

				if (getScope().equals(Scope.WACHTWOORDVERGETEN))
				{
					credentials = String.valueOf(authentication.getCredentials());
				}
				user = getUser(username);
			}
			else
			{
				String text = "Provider overgeslagen, verwacht scope " + getScope() + ", maar heeft scrope " + requestScope;
				LOG.debug(text);
				throw new UsernameNotFoundException(text);
			}

		}
		return user;
	}

	protected abstract UserDetails getUser(String username);

	protected abstract String getScope();

	public String getCredentials()
	{
		return credentials;
	}
}
