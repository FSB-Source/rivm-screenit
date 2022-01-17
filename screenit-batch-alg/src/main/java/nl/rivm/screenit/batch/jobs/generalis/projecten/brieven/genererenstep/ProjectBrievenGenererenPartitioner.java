package nl.rivm.screenit.batch.jobs.generalis.projecten.brieven.genererenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.util.query.ScreenitRestrictions;

import org.hibernate.Criteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.batch.item.ExecutionContext;

public class ProjectBrievenGenererenPartitioner extends AbstractBrievenGenererenPartitioner
{

	@Override
	protected void fillingData(Map<String, ExecutionContext> partities, ScreeningOrganisatie organisatie)
	{
		List<Long> projectBriefActies = getActieveProjectBriefActieDefinities(organisatie);
		for (Long projectBriefActieId : projectBriefActies)
		{
			ExecutionContext executionContext = new ExecutionContext();
			executionContext.put(ProjectBrievenConstants.KEY_PROJECT_SCREENINGORGANISATIE_ID, organisatie.getId());
			executionContext.put(ProjectBrievenConstants.KEY_PROJECT_ACTIE_ID, projectBriefActieId);
			partities.put(organisatie.getId().toString() + "_" + projectBriefActieId.toString(), executionContext);
		}
	}

	private List<Long> getActieveProjectBriefActieDefinities(ScreeningOrganisatie so)
	{
		Criteria crit = getHibernateSession().createCriteria(ProjectBrief.class);
		crit.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);

		crit.createAlias("definitie", "definitie");
		crit.createAlias("projectClient", "projectClient");
		crit.createAlias("projectClient.groep", "groep");
		crit.createAlias("projectClient.project", "project");

		crit.createAlias("client", "client");
		crit.createAlias("client.persoon", "persoon");
		crit.createAlias("persoon.gbaAdres", "adres");
		crit.createAlias("adres.gbaGemeente", "gemeente");
		crit.createAlias("gemeente.screeningOrganisatie", "org");

		crit.add(Restrictions.eq("org.id", so.getId()));

		ScreenitRestrictions.addClientBaseRestrictions(crit, "client", "persoon");

		crit.add(Restrictions.isNull("mergedBrieven"));

		crit.setProjection(Projections.property("definitie.id"));

		return crit.list();
	}
}
