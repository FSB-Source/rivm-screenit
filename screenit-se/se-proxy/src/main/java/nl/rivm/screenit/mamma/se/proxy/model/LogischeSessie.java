package nl.rivm.screenit.mamma.se.proxy.model;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDateTime;

import org.apache.commons.lang3.StringUtils;
import org.apache.shiro.codec.Base64;
import org.springframework.http.ResponseEntity;

public class LogischeSessie
{
	private String yubikeyIdentificatie;

	private NavigatieDto navigatie;

	private String credentials;

	private LocalDateTime laatsteUpdate;

	private ResponseEntity<String> loginAntwoord;

	public ResponseEntity<String> getLoginAntwoord()
	{
		return loginAntwoord;
	}

	public void setLoginAntwoord(ResponseEntity<String> loginAntwoord)
	{
		this.loginAntwoord = loginAntwoord;
	}

	public LogischeSessie(String credentials, String yubikeyIdentificatie)
	{
		this.credentials = credentials;
		this.yubikeyIdentificatie = yubikeyIdentificatie;
	}

	public NavigatieDto getNavigatie()
	{
		return navigatie;
	}

	public void setNavigatie(NavigatieDto navigatie)
	{
		this.navigatie = navigatie;
	}

	public String getCredentials()
	{
		return credentials;
	}

	public void setCredentials(String credentials)
	{
		this.credentials = credentials;
	}

	public LocalDateTime getLaatsteUpdate()
	{
		return laatsteUpdate;
	}

	public void setLaatsteUpdate(LocalDateTime laatsteUpdate)
	{
		this.laatsteUpdate = laatsteUpdate;
	}

	public String getYubikeyIdentificatie()
	{
		return yubikeyIdentificatie;
	}

	public void setYubikeyIdentificatie(String yubikeyIdentificatie)
	{
		this.yubikeyIdentificatie = yubikeyIdentificatie;
	}

	public String getGebruikersnaam()
	{
		return getCredentialArray()[0];
	}

	public String getWachtWoord()
	{
		return getCredentialArray()[1];
	}

	private String[] getCredentialArray()
	{
		String base64Credentials = Base64.decodeToString(getCredentials().substring(6));
		return StringUtils.split(base64Credentials, ":", 2);
	}
}
