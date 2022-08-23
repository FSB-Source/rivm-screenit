package nl.rivm.screenit.model;

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

import java.util.Arrays;
import java.util.List;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public enum Aanhef
{
	DHR("Dhr."),

	MEVR("Mevr."),

	GEACHTE_MEVROUW("Geachte mevrouw"),

	GEACHTE_HEER("Geachte heer"),

	GEACHTE("Geachte");

	private final String naam;

	public static Aanhef getAanhefWithName(String name)
	{
		for (Aanhef hef : Aanhef.values())
		{
			if (hef.getNaam().equals(name))
			{
				return hef;
			}
		}
		return null;
	}

	public static Aanhef bepaalJuisteAanhef(GbaPersoon persoon)
	{
		if (persoon.getAanhef() != null)
		{
			return persoon.getAanhef();
		}

		switch (persoon.getGeslacht())
		{
		case MAN:
			return GEACHTE_HEER;
		case VROUW:
			return GEACHTE_MEVROUW;
		case ONBEKEND:
		default:
			return GEACHTE;
		}
	}

	public static List<Aanhef> aanhefVormenClienten()
	{
		return Arrays.asList(Aanhef.GEACHTE_HEER, Aanhef.GEACHTE_MEVROUW, Aanhef.GEACHTE);
	}

	public static List<Aanhef> aanhefVormenMedewerkers()
	{
		return Arrays.asList(Aanhef.DHR, Aanhef.MEVR);
	}
}
