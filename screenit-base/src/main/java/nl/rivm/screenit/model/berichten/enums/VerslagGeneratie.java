package nl.rivm.screenit.model.berichten.enums;

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

import nl.rivm.screenit.model.berichten.VerslagProjectVersionMapping;

public enum VerslagGeneratie
{
	V1,
	V2,
	V3,
	V4,
	V5,
	V6,
	V7,
	V8,
	V10,
	V11;

	VerslagGeneratie()
	{
	}

	public boolean isHuidigeGeneratie(VerslagType verslagType)
	{
		return this.equals(getHuidigeGeneratie(verslagType));
	}

	public static VerslagGeneratie getHuidigeGeneratie(VerslagType verslagType)
	{
		return VerslagProjectVersionMapping.get().getHighestGeneratie(verslagType);
	}

	public String getVersionInConceptId()
	{
		return name().substring(1);
	}

	public String getXsltResource()
	{
		return "/Screenit-CDA" + getVersionInConceptId() + ".xsl";
	}

}
