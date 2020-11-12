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

import java.util.Arrays;
import java.util.List;

public enum MammaOnderzoekStatus
{
	ACTIEF,

	ONDERBROKEN,

	ONDERBROKEN_ZONDER_VERVOLG,

	ONVOLLEDIG,

	AFGEROND,

	;

	private static final List<MammaOnderzoekStatus> EINDSTATUSSEN = Arrays.asList(AFGEROND, ONDERBROKEN_ZONDER_VERVOLG, ONVOLLEDIG);

	public static boolean isEindstatus(MammaOnderzoekStatus status)
	{
		return EINDSTATUSSEN.contains(status);
	}
}
