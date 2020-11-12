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
import nl.rivm.screenit.model.INaam;

public enum MammaCapaciteitBlokType implements INaam
{

	REGULIER("Regulier", "#f7f2ff", "#d8beff", MammaFactorType.GEEN)
	{
		@Override
		public List<MammaDoelgroep> getDoelgroepen()
		{
			return Arrays.asList(MammaDoelgroep.REGULIER, MammaDoelgroep.DUBBELE_TIJD, MammaDoelgroep.MINDER_VALIDE);
		}
	},
	TEHUIS("Tehuis", "#fff297", "#ddd69f", MammaFactorType.DUBBELE_TIJD)
	{
		@Override
		public List<MammaDoelgroep> getDoelgroepen()
		{
			return Arrays.asList();
		}
	},

	GEEN_SCREENING("Geen screening", "#f4f4f4", "#c8c8c8", MammaFactorType.GEEN)
	{
		@Override
		public List<MammaDoelgroep> getDoelgroepen()
		{
			return Arrays.asList();
		}
	};

	private final String naam;

	private final String backgroundColor;

	private final String borderColor;

	private final MammaFactorType factorType;

	MammaCapaciteitBlokType(String naam, String backgroundColor, String borderColor, MammaFactorType factorType)
	{
		this.naam = naam;
		this.backgroundColor = backgroundColor;
		this.borderColor = borderColor;
		this.factorType = factorType;
	}

	@Override
	public String getNaam()
	{
		return naam;
	}

	public String getBackgroundColor()
	{
		return backgroundColor;
	}

	public String getBorderColor()
	{
		return borderColor;
	}

	public abstract List<MammaDoelgroep> getDoelgroepen();

	public MammaFactorType getFactorType()
	{
		return factorType;
	}
}
