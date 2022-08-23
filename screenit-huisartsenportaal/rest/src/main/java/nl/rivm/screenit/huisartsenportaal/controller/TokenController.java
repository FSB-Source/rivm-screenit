package nl.rivm.screenit.huisartsenportaal.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.security.oauth2.provider.token.TokenStore;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@RestController
@Transactional(propagation = Propagation.REQUIRED)
@AllArgsConstructor
public class TokenController
{

	private final TokenStore tokenStore;

	@RequestMapping(value = "/oauth/token/revoke", method = RequestMethod.POST)
	public void create(@RequestParam("token") String value)
	{
		var refreshToken = tokenStore.readRefreshToken(value);
		if (refreshToken != null)
		{
			tokenStore.removeRefreshToken(refreshToken);
			tokenStore.removeAccessTokenUsingRefreshToken(refreshToken);
		}
		else
		{
			var accessToken = tokenStore.readAccessToken(value);
			if (accessToken != null)
			{
				LOG.info("Token blijkt geen refresh-token te zijn maar een access-token");

				if (accessToken.getRefreshToken() != null)
				{
					tokenStore.removeRefreshToken(accessToken.getRefreshToken());
				}
				tokenStore.removeAccessToken(accessToken);
			}
		}
	}
}
