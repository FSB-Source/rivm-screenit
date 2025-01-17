package nl.rivm.screenit.model.mamma.enums;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Set;

public enum MammaAfspraakStatus
{
	GEPLAND, 

	VERPLAATST,

	UITGESTELD,

	INGESCHREVEN,

	ONDERZOEK,

	SIGNALEREN,

	BEEINDIGD,

	GEANNULEERD_CLIENT,

	GEANNULEERD_VIA_INFOLIJN,

	GEANNULEERD_VERHUIZING_BUITENLAND,

	GEANNULEERD_OVERLIJDEN,

	;

	public static final EnumSet<MammaAfspraakStatus> GEANNULEERD;

	public static final Set<MammaAfspraakStatus> NIET_GEANNULEERD;

	public static final EnumSet<MammaAfspraakStatus> GESTART;

	static
	{
		GEANNULEERD = EnumSet.of(GEANNULEERD_CLIENT, GEANNULEERD_VIA_INFOLIJN, GEANNULEERD_OVERLIJDEN, UITGESTELD, VERPLAATST, GEANNULEERD_VERHUIZING_BUITENLAND);
		GESTART = EnumSet.of(MammaAfspraakStatus.INGESCHREVEN, MammaAfspraakStatus.ONDERZOEK, MammaAfspraakStatus.SIGNALEREN, MammaAfspraakStatus.BEEINDIGD);
		NIET_GEANNULEERD = new HashSet<>(Arrays.asList(MammaAfspraakStatus.values()));
		NIET_GEANNULEERD.removeAll(GEANNULEERD);
	}

	public static boolean isGeannuleerd(MammaAfspraakStatus status)
	{
		return GEANNULEERD.contains(status);
	}

	public static boolean isGestart(MammaAfspraakStatus status)
	{
		return GESTART.contains(status);
	}
}
