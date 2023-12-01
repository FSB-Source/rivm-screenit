package nl.rivm.screenit.util.cervix;

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

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.huisartsenportaal.enums.CervixLocatieStatus;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixLocatieUtil
{

	public static boolean klantnummerNietGeverifieerd(CervixHuisartsLocatie locatie)
	{
		return CervixLocatieStatus.KLANTNUMMER_NIET_GEVERIFIEERD.equals(locatie.getStatus())
			|| Boolean.TRUE.equals(locatie.getMoetVerifierenVoorActivatie()) && CervixLocatieStatus.INACTIEF.equals(locatie.getStatus());
	}

}