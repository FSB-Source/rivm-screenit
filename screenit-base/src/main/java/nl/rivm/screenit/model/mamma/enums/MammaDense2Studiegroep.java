package nl.rivm.screenit.model.mamma.enums;

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

import lombok.Getter;
import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.model.OrganisatieParameterKey;

@Getter
@RequiredArgsConstructor
public enum MammaDense2Studiegroep
{
	CEM(OrganisatieParameterKey.MAMMA_DENSE2_CEM_PROJECT),
	MRI(OrganisatieParameterKey.MAMMA_DENSE2_MRI_PROJECT),
	CG(OrganisatieParameterKey.MAMMA_DENSE2_CONTROLEGROEP_PROJECT),
	CEM_HER(OrganisatieParameterKey.MAMMA_DENSE2_HERINNEREN_CEM_PROJECT),
	MRI_HER(OrganisatieParameterKey.MAMMA_DENSE2_HERINNEREN_MRI_PROJECT);

	private final OrganisatieParameterKey projectParameter;

}
