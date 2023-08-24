package nl.rivm.screenit.mamma.se.proxy.model;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
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

	private int dagenDaglijstOphalenLimiet;

	private boolean tomosyntheseMogelijk;

	public Boolean isNfcEnabled()
	{
		return nfcEnabled;
	}

	public void setNfcEnabled(String nfcEnabled)
	{
		this.nfcEnabled = nfcEnabled.equals("true");
	}

	public EnvironmentInfoDto(EnvironmentInfoDto environmentInfoDto)
	{
		this.version = environmentInfoDto.version;
		this.environment = environmentInfoDto.environment;
		this.timestamp = environmentInfoDto.timestamp;
		this.nfcEnabled = environmentInfoDto.nfcEnabled;
		this.huidigWerkstationIpAdres = environmentInfoDto.huidigWerkstationIpAdres;
		this.magUpdaten = environmentInfoDto.magUpdaten;
		this.dagenInDaglijstCache = environmentInfoDto.dagenInDaglijstCache;
		this.cacheVulling = environmentInfoDto.cacheVulling;
		this.dagenDaglijstOphalenLimiet = environmentInfoDto.dagenDaglijstOphalenLimiet;
		this.tomosyntheseMogelijk = environmentInfoDto.tomosyntheseMogelijk;
	}
}
