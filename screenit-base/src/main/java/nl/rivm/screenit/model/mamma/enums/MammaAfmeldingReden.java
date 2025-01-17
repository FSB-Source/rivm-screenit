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

import java.util.ArrayList;
import java.util.List;

public enum MammaAfmeldingReden
{
	MEDISCHE_REDEN(true, true),

	ANGST_VOOR_KANKER(true, true),

	ANGST_VOOR_STRALING(true, true),

	NARE_ERVARING_MET_ONDERZOEK(true, true),

	ONDERZOEK_NIET_NUTTIG(true, true),

	PIJNLIJKHEID_VORIG_ONDERZOEK(true, true),

	VERHINDERD(true, false),

	OVERIGE_REDENEN(true, true);

	private final boolean eenmalig;

	private final boolean definitief;

	private MammaAfmeldingReden(boolean eenmalig, boolean definitief)
	{
		this.eenmalig = eenmalig;
		this.definitief = definitief;
	}

	public static final List<MammaAfmeldingReden> eenmaligeRedenen()
	{
		List<MammaAfmeldingReden> redenen = new ArrayList<>();
		for (MammaAfmeldingReden reden : values())
		{
			if (reden.eenmalig)
			{
				redenen.add(reden);
			}
		}
		return redenen;
	}

	public static final List<MammaAfmeldingReden> definitieveRedenen()
	{
		List<MammaAfmeldingReden> redenen = new ArrayList<>();
		for (MammaAfmeldingReden reden : values())
		{
			if (reden.definitief)
			{
				redenen.add(reden);
			}
		}
		return redenen;
	}
}
