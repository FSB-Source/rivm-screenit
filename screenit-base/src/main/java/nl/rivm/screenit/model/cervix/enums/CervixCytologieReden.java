package nl.rivm.screenit.model.cervix.enums;

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

public enum CervixCytologieReden
{

	INITIEEL_NA_ZAS("0"),

	INITIEEL_ZONDER_ZAS("1"),

	HERHALING_INITIEEL_NA_ONBEOORDEELBAARHEID("2"),

	VERVOLGONDERZOEK("4"),

	HERHALING_VERVOLGONDERZOEK("7");

	private final String omlOrderCode;

	CervixCytologieReden(String omlOrderCode)
	{
		this.omlOrderCode = omlOrderCode;
	}

	public String getOmlOrderCode()
	{
		return omlOrderCode;
	}
}
