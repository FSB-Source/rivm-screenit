package nl.rivm.screenit.model.colon.enums;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

public enum ColonAfspraakStatus
{
	GEPLAND("Gepland"),

	UITGEVOERD("Uitgevoerd"),

	GEANNULEERD_CLIENT("Geannuleerd (door client)"),

	GEANNULEERD_VIA_INFOLIJN("Geannuleerd (via infolijn)"),

	GEANNULEERD_OVERLIJDEN("Geannuleerd ivm overlijden"),

	GEANNULEERD_AFMELDEN("Geannuleerd ivm afmelden"),

	GEANNULEERD_OPEN_UITNODIGING("Geannuleerd ivm open aanmaken uitnodiging"),

	GEANNULEERD_ONBEKEND("Geannuleerd onbekende reden"),

	VERPLAATST("Verplaatst"),

	;

	public static final EnumSet<ColonAfspraakStatus> VOOR_AGENDA = EnumSet.of(GEPLAND, UITGEVOERD);

	public static final EnumSet<ColonAfspraakStatus> GEANNULEERD = EnumSet.of(GEANNULEERD_OPEN_UITNODIGING, GEANNULEERD_CLIENT, GEANNULEERD_VIA_INFOLIJN, GEANNULEERD_OVERLIJDEN,
		GEANNULEERD_AFMELDEN, GEANNULEERD_ONBEKEND);

	private final String omschrijving;

	ColonAfspraakStatus(String omschrijving)
	{
		this.omschrijving = omschrijving;
	}

	@Override
	public String toString()
	{
		return this.omschrijving;
	}

	public static boolean isGeannuleerd(ColonAfspraakStatus status)
	{
		return GEANNULEERD.contains(status);
	}
}
