package nl.rivm.screenit.batch.jobs.generalis.projecten.brieven.genererenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import nl.rivm.screenit.batch.jobs.brieven.genereren.AbstractBrievenGenererenReader;
import nl.rivm.screenit.batch.jobs.generalis.projecten.brieven.ProjectBrievenConstants;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.specification.algemeen.ClientSpecification;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.algemeen.ProjectBriefSpecification.heeftDefinitieId;

@Component
public class ProjectBrievenGenererenReader extends AbstractBrievenGenererenReader<ProjectBrief>
{
	@Override
	protected Long getScreeningOrganisatieId()
	{
		return getStepExecutionContext().getLong(ProjectBrievenConstants.KEY_PROJECT_SCREENINGORGANISATIE_ID);
	}

	@Override
	protected Specification<ProjectBrief> createSpecification()
	{
		var specification = super.createSpecification();
		var projectBriefActieId = getStepExecutionContext().getLong(ProjectBrievenConstants.KEY_PROJECT_ACTIE_ID);
		return specification
			.and(heeftDefinitieId(projectBriefActieId))
			.and(ClientSpecification.heeftIndicatie().with(r -> clientJoin(r)));
	}
}
