package nl.rivm.screenit.mamma.se.proxy.model;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

public class AutorisatieDto
{
	private String displayName;

	private String username;

	private String medewerkercode;

	private String seCode;

	private String seNaam;

	private long instellingGebruikerId;

	private SERechtDto inschrijvenRecht;

	private SERechtDto onderzoekenRecht;

	private SERechtDto signalerenRecht;

	private SERechtDto kwaliteitsopnameRecht;

	private SERechtDto connectiestatusRecht;

	private String navigatie;

	private Map<SeConfiguratieKey, String> seParameters;

	public String getDisplayName()
	{
		return displayName;
	}

	public void setDisplayName(String displayName)
	{
		this.displayName = displayName;
	}

	public String getUsername()
	{
		return username;
	}

	public void setUsername(String username)
	{
		this.username = username;
	}

	public String getMedewerkercode()
	{
		return medewerkercode;
	}

	public void setMedewerkercode(String medewerkercode)
	{
		this.medewerkercode = medewerkercode;
	}

	public String getSeCode()
	{
		return seCode;
	}

	public void setSeCode(String seCode)
	{
		this.seCode = seCode;
	}

	public String getSeNaam()
	{
		return seNaam;
	}

	public void setSeNaam(String seNaam)
	{
		this.seNaam = seNaam;
	}

	public long getInstellingGebruikerId()
	{
		return instellingGebruikerId;
	}

	public void setInstellingGebruikerId(long instellingGebruikerId)
	{
		this.instellingGebruikerId = instellingGebruikerId;
	}

	public String getNavigatie()
	{
		return navigatie;
	}

	public void setNavigatie(String navigatie)
	{
		this.navigatie = navigatie;
	}

	public SERechtDto getInschrijvenRecht()
    {
		return inschrijvenRecht;
	}

	public void setInschrijvenRecht(SERechtDto inschrijvenRecht)
    {
		this.inschrijvenRecht = inschrijvenRecht;
	}

	public SERechtDto getOnderzoekenRecht()
    {
		return onderzoekenRecht;
	}

	public void setOnderzoekenRecht(SERechtDto onderzoekenRecht)
    {
		this.onderzoekenRecht = onderzoekenRecht;
	}

	public SERechtDto getSignalerenRecht()
    {
		return signalerenRecht;
	}

	public void setSignalerenRecht(SERechtDto signalerenRecht)
    {
		this.signalerenRecht = signalerenRecht;
	}

	public SERechtDto getKwaliteitsopnameRecht()
    {
		return kwaliteitsopnameRecht;
	}

	public void setKwaliteitsopnameRecht(SERechtDto kwaliteitsopnameRecht)
    {
		this.kwaliteitsopnameRecht = kwaliteitsopnameRecht;
	}

	public SERechtDto getConnectiestatusRecht()
    {
		return connectiestatusRecht;
	}

	public void setConnectiestatusRecht(SERechtDto connectiestatusRecht)
    {
		this.connectiestatusRecht = connectiestatusRecht;
	}

	public Map<SeConfiguratieKey, String> getSeParameters()
	{
		return seParameters;
	}

	public void setSeParameters(Map<SeConfiguratieKey, String> seParameters)
	{
		this.seParameters = seParameters;
	}
}
