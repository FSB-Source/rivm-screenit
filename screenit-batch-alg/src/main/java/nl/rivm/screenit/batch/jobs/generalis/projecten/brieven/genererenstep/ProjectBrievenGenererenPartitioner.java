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

import java.util.List;
import java.util.Map;

import nl.rivm.screenit.batch.jobs.brieven.genereren.AbstractBrievenGenererenPartitioner;
import nl.rivm.screenit.batch.jobs.generalis.projecten.brieven.ProjectBrievenConstants;
import nl.rivm.screenit.batch.repository.ProjectBriefRepository;
import nl.rivm.screenit.model.ScreeningOrganisatie;

import org.springframework.batch.item.ExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ProjectBrievenGenererenPartitioner extends AbstractBrievenGenererenPartitioner
{
	@Autowired
	private ProjectBriefRepository projectBriefRepository;

	@Override
	protected void fillingData(Map<String, ExecutionContext> partities, ScreeningOrganisatie organisatie)
	{
		List<Long> projectBriefActies = projectBriefRepository.getActieveProjectBriefActieDefinities(organisatie);
		for (Long projectBriefActieId : projectBriefActies)
		{
			ExecutionContext executionContext = new ExecutionContext();
			executionContext.put(ProjectBrievenConstants.KEY_PROJECT_SCREENINGORGANISATIE_ID, organisatie.getId());
			executionContext.put(ProjectBrievenConstants.KEY_PROJECT_ACTIE_ID, projectBriefActieId);
			partities.put(organisatie.getId().toString() + "_" + projectBriefActieId.toString(), executionContext);
		}
	}
}
