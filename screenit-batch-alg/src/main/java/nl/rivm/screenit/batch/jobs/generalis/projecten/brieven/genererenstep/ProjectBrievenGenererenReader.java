package nl.rivm.screenit.batch.jobs.generalis.projecten.brieven.genererenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import nl.rivm.screenit.batch.jobs.brieven.genereren.AbstractBrievenGenererenReader;
import nl.rivm.screenit.batch.jobs.generalis.projecten.brieven.ProjectBrievenConstants;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.project.ProjectBrief;

import org.hibernate.Criteria;
import org.hibernate.criterion.Restrictions;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.stereotype.Component;

@Component
public class ProjectBrievenGenererenReader extends AbstractBrievenGenererenReader<ProjectBrief>
{

	@Override
	protected Long getScreeningOrganisatieId(ExecutionContext context)
	{
		return context.getLong(ProjectBrievenConstants.KEY_PROJECT_SCREENINGORGANISATIE_ID);
	}

	@Override
	protected Criteria additionalRestrictions(Criteria crit, ExecutionContext context)
	{
		crit.add(Restrictions.eq("client.gbaStatus", GbaStatus.INDICATIE_AANWEZIG));
		Long projectBriefActieId = context.getLong(ProjectBrievenConstants.KEY_PROJECT_ACTIE_ID);
		crit.add(Restrictions.eq("definitie.id", projectBriefActieId));
		return crit;
	}
}
