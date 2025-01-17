package nl.rivm.screenit.model.mamma.dicom;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import org.apache.commons.lang.StringUtils;

public class SCPConfig
{
	private String aeTitle;

	private String host;

	private int poort;

	public String getAeTitle()
	{
		return aeTitle;
	}

	public String getHost()
	{
		return host;
	}

	public int getPoort()
	{
		return poort;
	}

	public static SCPConfig parse(String scpString, String defaultHost, Integer defaultPoort)
	{
		SCPConfig result = new SCPConfig();
		result.host = defaultHost;
		if (defaultPoort != null)
		{
			result.poort = defaultPoort;
		}
		String[] aeAtHostAndPort = StringUtils.split(scpString, ':');
		if (aeAtHostAndPort.length > 0)
		{
			String[] aeHost = StringUtils.split(aeAtHostAndPort[0], '@');
			result.aeTitle = aeHost[0];
			if (aeHost.length > 1)
			{
				result.host = aeHost[1];
			}
		}
		if (aeAtHostAndPort.length > 1 && StringUtils.isNumeric(aeAtHostAndPort[1]))
		{
			result.poort = Integer.parseInt(aeAtHostAndPort[1]);
		}
		return result;
	}
}
