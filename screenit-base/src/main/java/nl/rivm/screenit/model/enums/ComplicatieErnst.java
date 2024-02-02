package nl.rivm.screenit.model.enums;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

public enum ComplicatieErnst
{

	MILD("255604002", "2.16.840.1.113883.6.96"),

	MATIG("6736007", "2.16.840.1.113883.6.96"),

	ERNSTIG("24484000", "2.16.840.1.113883.6.96"),

	FATAAL("399166001", "2.16.840.1.113883.6.96");

	private final String code;

	private final String codeSysteem;

	private ComplicatieErnst(String code, String codeSysteem)
	{
		this.code = code;
		this.codeSysteem = codeSysteem;

	}

	public static ComplicatieErnst getValue(DSValue ernstIncidentcomplicatie)
	{
		if (ernstIncidentcomplicatie != null)
		{
			for (ComplicatieErnst value : values())
			{
				if (value.code.equals(ernstIncidentcomplicatie.getCode()) && value.codeSysteem.equals(ernstIncidentcomplicatie.getCodeSystem()))
				{
					return value;
				}
			}
		}
		return null;
	}
}
