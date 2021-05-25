package nl.rivm.screenit.model.colon;

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

import java.util.Arrays;

public enum ColonOnderzoeksVariant
{
	STANDAARD(IFOBTType.GOLD),

	@Deprecated
	TB_PAIRED(IFOBTType.GOLD, IFOBTType.EIKEN),

	VERGELIJKEND(IFOBTType.GOLD, IFOBTType.STUDIE);

	final private IFOBTType[] typen;

	ColonOnderzoeksVariant(IFOBTType... typen)
	{
		this.typen = typen;
	}

	public static boolean isOfType(ColonOnderzoeksVariant variant, IFOBTType type)
	{
		return variant != null && Arrays.asList(variant.typen).contains(type);
	}
}
