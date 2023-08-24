package nl.rivm.screenit.model.mamma.enums;

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

import java.util.EnumSet;

public enum MammaVerzettenReden
{
	CLIENTEN_PORTAAL,

	CLIENT_CONTACT,

	TECHNISCHE_STORING,

	LABORANT_ZIEK,

	ONVOORZIENE_OMSTANDIGHEDEN,

	PASSANT;

	public static final EnumSet<MammaVerzettenReden> BRIEF_VERPLICHT = EnumSet.of(LABORANT_ZIEK, TECHNISCHE_STORING, ONVOORZIENE_OMSTANDIGHEDEN);

	public static final boolean briefVerplicht(MammaVerzettenReden verzettenReden)
	{
		return verzettenReden != null && BRIEF_VERPLICHT.contains(verzettenReden);
	}

}
