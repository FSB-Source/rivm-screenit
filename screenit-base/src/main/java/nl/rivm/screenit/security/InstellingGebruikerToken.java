
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

import java.io.Serializable;

import org.apache.shiro.authc.AuthenticationToken;

public class InstellingGebruikerToken implements AuthenticationToken, BrowserOSToken
{

	private static final long serialVersionUID = 1L;

	private final Serializable id;

	private String browser;

	private String os;

	private String uzipasInlogMethode;

	public InstellingGebruikerToken(Serializable id)
	{
		this.id = id;
	}

	@Override
	public Object getPrincipal()
	{
		return id;
	}

	@Override
	public Object getCredentials()
	{
		return null;
	}

	public Serializable getId()
	{
		return id;
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
	public void setOs(String os)
	{
		this.os = os;
	}

	public String getUzipasInlogMethode()
	{
		return uzipasInlogMethode;
	}

	public void setUzipasInlogMethode(String uzipasInlogMethode)
	{
		this.uzipasInlogMethode = uzipasInlogMethode;
	}
}
