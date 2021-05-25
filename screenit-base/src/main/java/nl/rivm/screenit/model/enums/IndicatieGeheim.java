package nl.rivm.screenit.model.enums;

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

public enum IndicatieGeheim
{
	GEEN_BEPERKING("0"),

	NIET_ZONDER_TOESTEMMING("1"),

	NIET_AAN_KERKEN("2"),

	NIET_AAN_VRIJE_DERDEN("3"),

	NIET_ZONDER_TOESTEMMING_EN_NIET_AAN_KERKEN("4"),

	NIET_ZONDER_TOESTEMMING_EN_NIET_AAN_VRIJE_DERDEN("5"),

	NIET_AAN_KERKEN_EN_NIET_AAN_VRIJE_DERDEN("6"),

	NIET_ZONDER_TOESTEMMING_EN_NIET_AAN_VRIJE_DERDEN_EN_NIET_AAN_KERKEN("7");

	private final String code;

	private IndicatieGeheim(String code)
	{
		this.code = code;
	}

	public static IndicatieGeheim getByCode(String code)
	{
		for (IndicatieGeheim indicatieGeheim : IndicatieGeheim.values())
		{
			if (indicatieGeheim.code.equals(code))
			{
				return indicatieGeheim;
			}
		}

		return null;
	}

	public String getCode()
	{
		return code;
	}

}
