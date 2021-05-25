
package nl.rivm.screenit.model;

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

public enum RedenOpnieuwAanvragenClientgegevens
{

	ONJUIST_ADRES("Onjuist adres"),

	ONJUISTE_PERSOONSGEGEVENS("Onjuiste persoonsgegevens"),

	BEZWAAR_INGETROKKEN("Bezwaar uitwisseling BRP ingetrokken");

	private final String omschrijving;

	private RedenOpnieuwAanvragenClientgegevens(String omschrijving)
	{
		this.omschrijving = omschrijving;
	}

	@Override
	public String toString()
	{
		return this.omschrijving;
	}

	public String getNaam()
	{
		return omschrijving;
	}
}
