package nl.rivm.screenit.model.enums;

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

import nl.rivm.screenit.model.verslag.DSValue;

public enum ComplicatieSoort
{

	PERFORATIE("9", "2.16.840.1.113883.2.4.3.36.77.5.10"),

	BLOEDING("10", "2.16.840.1.113883.2.4.3.36.77.5.10"),

	OVERIG("OTH", "2.16.840.1.113883.5.1008");

	private final String code;

	private final String codeSysteem;

	private ComplicatieSoort(String code, String codeSysteem)
	{
		this.code = code;
		this.codeSysteem = codeSysteem;

	}

	public static ComplicatieSoort getValue(DSValue typeIncidentcomplicatie)
	{
		if (typeIncidentcomplicatie != null)
		{
			for (ComplicatieSoort value : values())
			{
				if (value.code.equals(typeIncidentcomplicatie.getCode()) && value.codeSysteem.equals(typeIncidentcomplicatie.getCodeSystem()))
				{
					return value;
				}
			}
		}
		return null;
	}
}
