package nl.rivm.screenit.model.colon.enums;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

@Deprecated
public enum ColonCTColografieReden
{
	EERDERE_ONVOLLEDIGE_COLOSCOPIE("Eerdere onvolledige coloscopie"),

	ANDERE_MEDISCHE_REDEN("Andere fysiek-medische reden"),

	PSYCHISCH_MEDISCHE_REDEN("Psychisch-medische reden"),

	PATIENT_WIGERT_COLOSCOPIE("Pati\u00EBnt weigert coloscopie"),

	ANDERS_NAMELIJK("Anders, namelijk ...");

	private final String omschrijving;

	private ColonCTColografieReden(String omschrijving)
	{
		this.omschrijving = omschrijving;

	}

	public String getOmschrijving()
	{
		return this.omschrijving;
	}

}
