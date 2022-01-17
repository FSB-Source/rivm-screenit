
package nl.rivm.screenit.model.colon.enums;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

public enum SKMLSentineelControleType
{
	HOOG(1, "Hoog"),

	MIDDEN(1, "Middel"),

	LAAG(1, "Laag"),

	HOOG_NIEUW(2, "Hoog"),

	MIDDEN_NIEUW(2, "Middel"),

	LAAG_NIEUW(2, "Laag");

	private final String korteOmschrijving;

	private int setNr;

	SKMLSentineelControleType(int setNr, String korteOmschrijving)
	{
		this.setNr = setNr;
		this.korteOmschrijving = korteOmschrijving;
	}

	public String getKorteOmschrijving()
	{
		return korteOmschrijving;
	}

	public String getOmschrijving()
	{
		return korteOmschrijving + " set " + setNr;
	}

	public int getSetNr()
	{
		return setNr;
	}
}
