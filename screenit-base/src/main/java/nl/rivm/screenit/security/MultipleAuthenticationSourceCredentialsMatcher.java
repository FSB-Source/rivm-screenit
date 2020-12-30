
package nl.rivm.screenit.security;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import nl.rivm.screenit.Constants;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.yubikey.shiro.YubikeyAuthenticationInfo;
import nl.topicuszorg.yubikey.shiro.YubikeyMatcher;
import nl.topicuszorg.yubikey.shiro.YubikeyToken;

import org.apache.shiro.authc.AuthenticationInfo;
import org.apache.shiro.authc.AuthenticationToken;
import org.apache.shiro.authc.UsernamePasswordToken;
import org.apache.shiro.authc.credential.CredentialsMatcher;
import org.apache.shiro.authc.credential.HashedCredentialsMatcher;
import org.apache.shiro.crypto.hash.Sha512Hash;

public class MultipleAuthenticationSourceCredentialsMatcher implements CredentialsMatcher
{

	private final HashedCredentialsMatcher passwordCredentialsMatcher;

	private final YubikeyMatcher yubikeyMatcher;

	private final HibernateService hibernateService;

	public MultipleAuthenticationSourceCredentialsMatcher(HibernateService hibernateService)
	{
		passwordCredentialsMatcher = new HashedCredentialsMatcher();
		passwordCredentialsMatcher.setHashAlgorithmName(Sha512Hash.ALGORITHM_NAME);
		passwordCredentialsMatcher.setHashIterations(Constants.PASSWORDHASHINGITERATIONS);
		passwordCredentialsMatcher.setStoredCredentialsHexEncoded(Boolean.TRUE);

		yubikeyMatcher = new YubikeyMatcher();
		yubikeyMatcher.setHashAlgorithm(Sha512Hash.ALGORITHM_NAME);
		yubikeyMatcher.setHashIterations(Constants.PASSWORDHASHINGITERATIONS);

		this.hibernateService = hibernateService;
	}

	@Override
	public boolean doCredentialsMatch(AuthenticationToken token, AuthenticationInfo info)
	{
		if (token instanceof UsernamePasswordToken)
		{
			boolean result = passwordCredentialsMatcher.doCredentialsMatch(token, info);

			if (result && token instanceof YubikeyToken)
			{
				YubikeyAuthenticationInfo yubikeyAuthenticationInfo = (YubikeyAuthenticationInfo) info;
				result = yubikeyMatcher.doCredentialsMatch(token, info);
				hibernateService.saveOrUpdate(yubikeyAuthenticationInfo.getYubiKey());
			}

			return result;
		}
		else if (token instanceof DigiDToken || token instanceof InstellingGebruikerToken || token instanceof UziToken)
		{

			return true;
		}

		return false;
	}

}
