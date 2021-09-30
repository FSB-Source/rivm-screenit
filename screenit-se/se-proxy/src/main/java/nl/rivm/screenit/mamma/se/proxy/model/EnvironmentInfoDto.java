package nl.rivm.screenit.mamma.se.proxy.model;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

public class EnvironmentInfoDto
{
	private String version;

	private String environment;

	private String timestamp;

	private Boolean nfcEnabled;

	private String huidigWerkstationIpAdres;

	private Boolean magUpdaten;

	private String dagenInDaglijstCache;

	private String cacheVulling;

	public EnvironmentInfoDto()
	{
	}

	public Boolean getMagUpdaten()
	{
		return magUpdaten;
	}

	public void setMagUpdaten(Boolean magUpdaten)
	{
		this.magUpdaten = magUpdaten;
	}

	public String getVersion()
	{
		return version;
	}

	public void setVersion(String version)
	{
		this.version = version;
	}

	public String getEnvironment()
	{
		return environment;
	}

	public void setEnvironment(String environment)
	{
		this.environment = environment;
	}

	public String getTimestamp()
	{
		return timestamp;
	}

	public void setTimestamp(String timestamp)
	{
		this.timestamp = timestamp;
	}

	public Boolean isNfcEnabled()
	{
		return nfcEnabled;
	}

	public void setNfcEnabled(String nfcEnabled)
	{
		this.nfcEnabled = nfcEnabled.equals("true");
	}

	public String getHuidigWerkstationIpAdres()
	{
		return huidigWerkstationIpAdres;
	}

	public void setHuidigWerkstationIpAdres(String huidigWerkstationIpAdres)
	{
		this.huidigWerkstationIpAdres = huidigWerkstationIpAdres;
	}

	public String getDagenInDaglijstCache()
	{
		return dagenInDaglijstCache;
	}

	public void setDagenInDaglijstCache(String dagenInDaglijstCache)
	{
		this.dagenInDaglijstCache = dagenInDaglijstCache;
	}

	public String getCacheVulling()
	{
		return cacheVulling;
	}

	public void setCacheVulling(String cacheVulling)
	{
		this.cacheVulling = cacheVulling;
	}
}
