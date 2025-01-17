package nl.rivm.screenit.model.cervix.berichten;

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

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum CervixHpvResultCode
{
	HR("02HPVHR", CervixHpvOrderCode.PAN),

	OHR("02HPVOHR", CervixHpvOrderCode.GEN),
	HPV16("02HPV16", CervixHpvOrderCode.GEN),
	HPV18("02HPV18", CervixHpvOrderCode.GEN),
	;

	private String berichtWaarde;

	private CervixHpvOrderCode orderCode;

	public static CervixHpvResultCode fromBerichtWaarde(String berichtWaarde)
	{
		return Arrays.stream(CervixHpvResultCode.values()).filter(oc -> oc.getBerichtWaarde().equals(berichtWaarde)).findFirst().orElse(null);
	}
}
