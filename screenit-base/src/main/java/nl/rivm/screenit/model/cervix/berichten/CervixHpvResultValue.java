package nl.rivm.screenit.model.cervix.berichten;

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

import lombok.AllArgsConstructor;
import lombok.Getter;

import nl.rivm.screenit.model.cervix.enums.CervixHpvBeoordelingWaarde;

@Getter
@AllArgsConstructor
public enum CervixHpvResultValue
{
	POS_HR_HPV("POS HR HPV", CervixHpvResultCode.HR, null),
	NEG_HR_HPV("NEG HR HPV", CervixHpvResultCode.HR, null),
	INVALID_HR_HPV("Invalid HR HPV", CervixHpvResultCode.HR, null),

	POS_OTHER_HR_HPV("POS Other HR HPV", CervixHpvResultCode.OHR, CervixHpvBeoordelingWaarde.POSITIEF),
	NEG_OTHER_HR_HPV("NEG Other HR HPV", CervixHpvResultCode.OHR, CervixHpvBeoordelingWaarde.NEGATIEF),
	INVALID_OTHER_HR_HPV("Invalid Other HR HPV", CervixHpvResultCode.OHR, CervixHpvBeoordelingWaarde.ONGELDIG),

	POS_HPV16("POS HPV16", CervixHpvResultCode.HPV16, CervixHpvBeoordelingWaarde.POSITIEF),
	NEG_HPV16("NEG HPV16", CervixHpvResultCode.HPV16, CervixHpvBeoordelingWaarde.NEGATIEF),
	INVALID_HPV16("Invalid HPV16", CervixHpvResultCode.HPV16, CervixHpvBeoordelingWaarde.ONGELDIG),

	POS_HPV18("POS HPV18", CervixHpvResultCode.HPV18, CervixHpvBeoordelingWaarde.POSITIEF),
	NEG_HPV18("NEG HPV18", CervixHpvResultCode.HPV18, CervixHpvBeoordelingWaarde.NEGATIEF),
	INVALID_HPV18("Invalid HPV18", CervixHpvResultCode.HPV18, CervixHpvBeoordelingWaarde.ONGELDIG),

	FAILURE("Failed", null, null),
	;

	private String berichtWaarde;

	private CervixHpvResultCode resultCode;

	private CervixHpvBeoordelingWaarde resultaatType;

	public static CervixHpvResultValue fromValue(String value)
	{
		for (CervixHpvResultValue waarde : CervixHpvResultValue.values())
		{
			if (waarde.getBerichtWaarde().equals(value))
			{
				return waarde;
			}
		}
		return null;
	}
}
