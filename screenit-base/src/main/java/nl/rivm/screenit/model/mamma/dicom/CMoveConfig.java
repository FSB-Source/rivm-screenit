package nl.rivm.screenit.model.mamma.dicom;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

public class CMoveConfig
{
	private SCPConfig queryRetrieve;

	private SCPConfig store;

	public SCPConfig getQueryRetrieve()
	{
		return queryRetrieve;
	}

	public SCPConfig getStore()
	{
		return store;
	}

	public static CMoveConfig parse(String moveConnectionsString)
	{
		CMoveConfig result = new CMoveConfig();
		String moveConnections[] = moveConnectionsString.split(",");
		result.queryRetrieve = SCPConfig.parse(moveConnections[0], "localhost", 11112);
		result.store = SCPConfig.parse(moveConnections[1], "localhost", 11113);
		return result;
	}
}
