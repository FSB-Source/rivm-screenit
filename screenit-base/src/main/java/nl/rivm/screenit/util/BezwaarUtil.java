package nl.rivm.screenit.util;

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

import java.util.Arrays;

import nl.rivm.screenit.model.Bezwaar;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BezwaarType;

public class BezwaarUtil
{

	private BezwaarUtil()
	{
	}

	public static boolean isBezwaarActiefVoorEenVanDeOnderzoeken(Client client, BezwaarType type)
	{
		if (type.getBevolkingsonderzoeken().length == 0)
		{
			return isBezwaarActiefVoor(client, type);
		}
		else
		{
			return Arrays.asList(type.getBevolkingsonderzoeken()).stream().anyMatch(onderzoek -> isBezwaarActiefVoor(client, type, onderzoek));
		}
	}

	public static boolean isBezwaarActiefVoor(Client client, BezwaarType type)
	{
		return isBezwaarActiefVoor(client, type, null);
	}

	public static boolean isBezwaarActiefVoor(Client client, BezwaarType type, Bevolkingsonderzoek onderzoek)
	{
		if (client != null)
		{
			return isBezwaarActiefVoor(client.getLaatstVoltooideBezwaarMoment(), type, onderzoek, true);
		}
		return false;
	}

	public static boolean isBezwaarActiefVoor(BezwaarMoment moment, BezwaarType type, Bevolkingsonderzoek onderzoek, boolean checkDossierBezwaar)
	{
		if (moment != null)
		{
			for (Bezwaar bezwaar : moment.getBezwaren())
			{
				if (type.equals(bezwaar.getType()) && (!type.equals(BezwaarType.VERZOEK_TOT_VERWIJDERING_DOSSIER) || !checkDossierBezwaar)
					&& (onderzoek == null && bezwaar.getBevolkingsonderzoek() == null || onderzoek != null && onderzoek.equals(bezwaar.getBevolkingsonderzoek())))
				{
					return true;
				}
			}
		}
		return false;
	}

	public static boolean isVerwijderDossierHetEnigeBezwaar(BezwaarMoment moment)
	{
		if (moment == null || moment.getBezwaren().isEmpty())
		{
			return false;
		}
		for (Bezwaar bezwaar : moment.getBezwaren())
		{
			if (!BezwaarType.VERZOEK_TOT_VERWIJDERING_DOSSIER.equals(bezwaar.getType()))
			{
				return false;
			}
		}
		return true;
	}
}
