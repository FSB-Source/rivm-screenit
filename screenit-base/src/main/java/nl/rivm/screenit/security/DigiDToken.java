
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

import org.apache.shiro.authc.AuthenticationToken;

public class DigiDToken implements AuthenticationToken, BrowserOSToken
{
	
	private static final long serialVersionUID = 1L;

	private final String bsn;

	private String browser;

	private String os;

	public DigiDToken(String bsn)
	{
		super();
		this.bsn = bsn;
	}

	@Override
	public Object getPrincipal()
	{
		return bsn;
	}

	@Override
	public Object getCredentials()
	{
		return null;
	}

	public String getBsn()
	{
		return bsn;
	}

	@Override
	public String getBrowser()
	{
		return browser;
	}

	@Override
	public void setBrowser(String browser)
	{
		this.browser = browser;
	}

	@Override
	public String getOs()
	{
		return os;
	}

	@Override
	public void setOs(String OS)
	{
		os = OS;
	}
}
