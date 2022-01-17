
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

public enum RedenAfspraakAfzeggen
{

	CLIENT_WIL_NIET_DEELNEMEN("Cli\u00EBnt wil niet deelnemen"),

	HUISARTS_ADVISEERT_ANNULERING("Huisarts adviseert annulering"),

	CLIENT_OVERLEDEN("Cli\u00EBnt is overleden");

	private final String omschrijving;

	RedenAfspraakAfzeggen(String omschrijving)
	{
		this.omschrijving = omschrijving;
	}

	@Override
	public String toString()
	{
		return omschrijving;
	}
}
