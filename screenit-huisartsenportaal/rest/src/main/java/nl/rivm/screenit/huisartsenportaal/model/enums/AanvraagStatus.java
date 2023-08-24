package nl.rivm.screenit.huisartsenportaal.model.enums;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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

@AllArgsConstructor
@Getter
public enum AanvraagStatus
{

	AANGEVRAAGD("Aangevraagd"),

	AFGEDRUKT_KLAAR_OM_TE_VERSTUREN("Afgedrukt"),

	AFGEDRUKT_EN_VERSTUURD("Verstuurd"),

	VERWIJDERD("Verwijderd");

	private final String naam;

	public static AanvraagStatus getAanvraagStatusByName(String naam)
	{
		for (AanvraagStatus status : AanvraagStatus.values())
		{
			if (status.getNaam().equals(naam))
			{
				return status;
			}
		}
		return null;
	}
}
