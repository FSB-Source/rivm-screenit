package nl.rivm.screenit.batch.jobs.cervix.verlatedeelnamecovid.step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.cervix.verlatedeelnamecovid.CervixVerlateDeelnameCovidConstants;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.project.GroepInvoer;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.rivm.screenit.service.BriefHerdrukkenService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.ProjectService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.stereotype.Component;

@Component
@Slf4j
@AllArgsConstructor
public class CervixVerlateDeelnameCovidWriter extends BaseWriter<CervixBrief>
{
	private final BriefHerdrukkenService briefHerdrukkenService;

	private final ProjectService projectService;

	private final HibernateService hibernateService;

	private final LogService logService;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	protected void write(CervixBrief brief) throws Exception
	{
		var projectGroep = prepareProjectGroep();
		try
		{
			LOG.info("Voeg client (clientId: " + brief.getClient().getId() + ") toe aan project groep '" + projectGroep.getNaam() + "'");
			var projectClient = projectService.addClientToProjectGroep(projectGroep, brief.getClient());
			brief.setAangevraagdeHerdruk(true);
			briefHerdrukkenService.opnieuwAanmaken(brief, null);
			projectClient.setActief(false);
			projectClient.setProjectInactiefDatum(currentDateSupplier.getDate());
			hibernateService.saveOrUpdate(projectClient);
		}
		catch (IllegalStateException e)
		{
			var melding = e.getMessage() + ". Geen herdruk van laatste uitnodiging aangemaakt.";
			LOG.warn(melding);
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_CLIENT_GEEN_VERLATE_DEELNAME_UITNODIGING, brief.getClient(), melding, Bevolkingsonderzoek.CERVIX);
		}
	}

	private ProjectGroep prepareProjectGroep()
	{
		ProjectGroep projectGroep;
		var context = getExecutionContext();
		if (!context.containsKey(CervixVerlateDeelnameCovidConstants.PROJECT_GROEP_ID))
		{
			String groepsNaam = "Populatie van run " + DateUtil.formatShortDate(currentDateSupplier.getDate());
			Long projectId = context.getLong(CervixVerlateDeelnameCovidConstants.PROJECT_ID);
			var project = hibernateService.load(Project.class, projectId);
			LOG.info("Maak project groep " + groepsNaam + " voor project '" + project.getNaam() + "'");
			projectGroep = new ProjectGroep();
			projectGroep.setProject(project);
			project.getGroepen().add(projectGroep);
			projectGroep.setActief(true);
			projectGroep.setGroepInvoer(GroepInvoer.CRITERIA);
			projectGroep.setActiefDatum(currentDateSupplier.getDate());
			projectGroep.setNaam(groepsNaam);
			projectGroep.setPopulatie(0);
			hibernateService.saveOrUpdate(projectGroep);
			context.put(CervixVerlateDeelnameCovidConstants.PROJECT_GROEP_ID, projectGroep.getId());
		}
		else
		{
			projectGroep = hibernateService.load(ProjectGroep.class, context.getLong(CervixVerlateDeelnameCovidConstants.PROJECT_GROEP_ID));
		}
		return projectGroep;
	}
}
