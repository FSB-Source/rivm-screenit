package nl.rivm.screenit.model.mamma;

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

public class DicomCStoreConfig
{
	private DicomSCPConfig scuConfig;

	private DicomSCPConfig scpConfig;

	public DicomSCPConfig getScuConfig()
	{
		return scuConfig;
	}

	public void setScuConfig(DicomSCPConfig scuConfig)
	{
		this.scuConfig = scuConfig;
	}

	public DicomSCPConfig getScpConfig()
	{
		return scpConfig;
	}

	public void setScpConfig(DicomSCPConfig scpConfig)
	{
		this.scpConfig = scpConfig;
	}

	public static DicomCStoreConfig parse(String moveConnectionsString)
	{
        DicomCStoreConfig result = new DicomCStoreConfig();
		String moveConnections[] = moveConnectionsString.split(",");
		result.scuConfig = DicomSCPConfig.parse(moveConnections[0], "localhost", 11112);
		result.scpConfig = DicomSCPConfig.parse(moveConnections[1], "localhost", 11113);
		return result;
	}
}
