package nl.rivm.screenit.model.mamma.enums;

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

public enum SuboptimaleInsteltechniek implements INaam
{
	FYSIEK_BEPERKT("Fysiek beperkt"),

	MOBIEL_BEPERKT("Mobiel beperkt"),

	MOEILIJK_TE_POSITIONEREN("Moeilijk te positioneren");

	private String naam;

	SuboptimaleInsteltechniek(String naam)
	{
		this.naam = naam;
	}

	public String getNaam()
	{
		return naam;
	}
}
