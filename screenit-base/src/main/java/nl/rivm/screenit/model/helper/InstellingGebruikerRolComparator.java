package nl.rivm.screenit.model.helper;

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

import java.io.Serializable;
import java.util.Comparator;

import nl.rivm.screenit.model.InstellingGebruikerRol;

public class InstellingGebruikerRolComparator implements Comparator<InstellingGebruikerRol>, Serializable
{

	private static final long serialVersionUID = 1L;

	@Override
	public int compare(InstellingGebruikerRol rolVanDezeGebruiker, InstellingGebruikerRol rolVanAndereGebruiker)
	{
		if (rolVanDezeGebruiker.getId() != null && rolVanAndereGebruiker.getId() == null)
		{
			return 1;
		}
		if (rolVanDezeGebruiker.getId() == null && rolVanAndereGebruiker.getId() != null)
		{
			return -1;
		}

		String naamVanDezeGebruiker = "";
		String naamVanAndereGebruiker = "";
		if (rolVanDezeGebruiker.getRol() != null)
		{
			naamVanDezeGebruiker = rolVanDezeGebruiker.getRol().getNaam();
		}
		if (rolVanAndereGebruiker.getRol() != null)
		{
			naamVanAndereGebruiker = rolVanAndereGebruiker.getRol().getNaam();
		}
		return naamVanDezeGebruiker.compareTo(naamVanAndereGebruiker);
	}

}
