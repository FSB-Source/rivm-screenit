package nl.rivm.screenit.model;

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

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.enums.BriefType;

@Getter
@AllArgsConstructor
@NoArgsConstructor
public enum ClientContactManier
{
	FORMULIER_VOOR_AANVRAGEN(BriefType.CLIENT_BEZWAAR_AANVRAAG),

	FORMULIER_VOOR_INTREKKEN(BriefType.CLIENT_BEZWAAR_INTREKKEN),

	FORMULIER_VOOR_ONDERZOEKSRESULTATEN_VERWIJDEREN(BriefType.CLIENT_BEZWAAR_VERWIJDEREN_ONDERZOEKRESULTATEN),

	FORMULIER_VOOR_ALLES_VERWIJDEREN(BriefType.CLIENT_BEZWAAR_ALLES_VERWIJDEREN),

	DIRECT;

	private BriefType briefType;

	public static final List<ClientContactManier> AANVRAGEN_FORMULIEREN = List.of(FORMULIER_VOOR_AANVRAGEN, FORMULIER_VOOR_INTREKKEN,
		FORMULIER_VOOR_ONDERZOEKSRESULTATEN_VERWIJDEREN, FORMULIER_VOOR_ALLES_VERWIJDEREN);
}
