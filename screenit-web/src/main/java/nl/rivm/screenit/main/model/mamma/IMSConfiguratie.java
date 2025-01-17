package nl.rivm.screenit.main.model.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class IMSConfiguratie implements Serializable
{
	private String hostName;

	private int ormPort;

	private int adtPort;

	private int ilmPort;

	private int bezwaarTermijnVerwijderdeBeelden;

	private int imsQueueSizeWarningThreshold;

	public IMSConfiguratie()
	{
	}

	public IMSConfiguratie(String hostName, int ormPort, int adtPort, int ilmPort, int bezwaarTermijnVerwijderdeBeelden, int imsQueueSizeWarningThreshold)
	{
		this.hostName = hostName;
		this.ormPort = ormPort;
		this.adtPort = adtPort;
		this.ilmPort = ilmPort;
		this.bezwaarTermijnVerwijderdeBeelden = bezwaarTermijnVerwijderdeBeelden;
		this.imsQueueSizeWarningThreshold = imsQueueSizeWarningThreshold;
	}
}
