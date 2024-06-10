package nl.rivm.screenit.model.project;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.model.enums.Recht;

@RequiredArgsConstructor(access = AccessLevel.PRIVATE)
@Getter
public enum ProjectType
{
	BRIEFPROJECT("Briefproject", Recht.GEBRUIKER_BRIEFPROJECT_OVERZICHT),

	PROJECT("Project", Recht.GEBRUIKER_PROJECT_OVERZICHT),
	;

	private final String naam;

	private final Recht recht;

	@Override
	public String toString()
	{
		return naam;
	}
}
