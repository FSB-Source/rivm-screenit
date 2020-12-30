package nl.rivm.screenit.model.enums;

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

import nl.rivm.screenit.model.INaam;

public enum ColonTest implements INaam
{

	U1(Boolean.TRUE),

	U2_1(Boolean.TRUE),

	U2_2(Boolean.FALSE),

	U2_3(Boolean.TRUE),

	U3_1(Boolean.TRUE),

	U3_2(Boolean.TRUE),

	U3_3(Boolean.TRUE),

	U3_4(Boolean.TRUE),

	U3_5(Boolean.TRUE),

	U3_6(Boolean.TRUE),

	U3_7(Boolean.TRUE),

	U4(Boolean.TRUE),

	U6(Boolean.TRUE),

	;

	private Boolean actief;

	ColonTest(Boolean actief)
	{
		this.actief = actief;
	}

	@Override
	public String getNaam()
	{
		return this.name();
	}

	public Boolean getActief()
	{
		return actief;
	}

}
