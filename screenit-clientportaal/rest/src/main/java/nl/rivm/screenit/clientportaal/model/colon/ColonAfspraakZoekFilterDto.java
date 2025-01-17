package nl.rivm.screenit.clientportaal.model.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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

import java.time.LocalDate;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import nl.rivm.screenit.clientportaal.model.ClientportaalBaseDto;

@Getter
@Setter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class ColonAfspraakZoekFilterDto extends ClientportaalBaseDto
{
	private String ziekenhuisnaam;

	private LocalDate vanaf;

	private LocalDate totEnMet;

	private String plaats;

	private Integer afstand;

	private Integer pagecount;

	private Integer maxResultsPerSearchInteration;
}
