package nl.rivm.screenit.model;

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

import java.math.BigDecimal;

import lombok.AllArgsConstructor;
import lombok.Getter;

import nl.rivm.screenit.model.colon.ColonOnderzoeksVariant;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.project.ProjectType;

@Getter
@AllArgsConstructor
public enum ProjectParameterKey
{

	COLON_ONDERZOEKSVARIANT(ProjectType.PROJECT, ColonOnderzoeksVariant.class, Bevolkingsonderzoek.COLON, null, false),

	COLON_UITNODIGEN_PRIORITEIT(ProjectType.PROJECT, Integer.class, Bevolkingsonderzoek.COLON, 999, false),

	COLON_FIT_NORM_WAARDE(ProjectType.PROJECT, BigDecimal.class, Bevolkingsonderzoek.COLON, 1000, false),

	COLON_WACHTTIJD_UITSLAG_STUDIETEST(ProjectType.PROJECT, Integer.class, Bevolkingsonderzoek.COLON, 999, false),

	COLON_AFWIJKING_UITNODIGINGSINTERVAL(ProjectType.PROJECT, Integer.class, Bevolkingsonderzoek.COLON, -5, 5, false);

	private final ProjectType projectType;

	private final Class<?> valueType;

	private final Bevolkingsonderzoek bevolkingsonderzoek;

	private final Integer minValue;

	private final Integer maxValue;

	private final boolean isUniek;

	ProjectParameterKey(ProjectType projectType, Class<?> valueType, Bevolkingsonderzoek bevolkingsonderzoek, Integer maxValue, boolean isUniek)
	{
		this(projectType, valueType, bevolkingsonderzoek, 0, maxValue, isUniek);
	}
}
