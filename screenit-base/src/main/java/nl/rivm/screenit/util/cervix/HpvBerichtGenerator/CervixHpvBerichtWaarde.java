package nl.rivm.screenit.util.cervix.HpvBerichtGenerator;

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

import nl.rivm.screenit.model.cervix.enums.CervixHpvUitslag;

public enum CervixHpvBerichtWaarde
{
	POS_HR_HPV(CervixHpvUitslag.POSITIEF, "POS HR HPV"),

	NEG_HR_HPV(CervixHpvUitslag.NEGATIEF, "NEG HR HPV"),

	INVALID_HR_HPV(CervixHpvUitslag.ONGELDIG, "Invalid HR HPV"),

	FAILURE("Failed"),

	POS_OTHER_HR_HPV("POS Other HR HPV"),
	NEG_OTHER_HR_HPV("NEG Other HR HPV"),
	INVALID_OTHER_HR("Invalid Other HR"),
	HPV("HPV"),
	POS_HPV16("POS HPV16"),
	NEG_HPV16("NEG HPV16"),
	INVALID_HPV16("Invalid HPV16"),
	POS_HPV18("POS HPV18"),
	NEG_HPV18("NEG HPV18"),
	INVALID_HPV18("Invalid HPV18"),
	;

	private String berichtWaarde;

	private CervixHpvUitslag uitslag;

	private CervixHpvBerichtWaarde(CervixHpvUitslag uitslag, String berichtWaarde)
	{
		this.uitslag = uitslag;
		this.berichtWaarde = berichtWaarde;
	}

	private CervixHpvBerichtWaarde(String berichtWaarde)
	{
		this.uitslag = null;
		this.berichtWaarde = berichtWaarde;
	}

	public String getBerichtWaarde()
	{
		return berichtWaarde;
	}

	public void setBerichtWaarde(String berichtWaarde)
	{
		this.berichtWaarde = berichtWaarde;
	}

	public CervixHpvUitslag getUitslag()
	{
		return uitslag;
	}

	public void setUitslag(CervixHpvUitslag uitslag)
	{
		this.uitslag = uitslag;
	}

	public static CervixHpvBerichtWaarde fromValue(String value)
	{
		for (CervixHpvBerichtWaarde waarde : CervixHpvBerichtWaarde.values())
		{
			if (waarde.getBerichtWaarde().equals(value))
			{
				return waarde;
			}
		}
		return null;
	}
}
