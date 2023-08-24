package nl.rivm.screenit.batch.jobs.cervix.oudenietverstuurdezas.step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.batch.jobs.cervix.oudenietverstuurdezas.CervixOudeNietIngestuurdeZasConstants;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.project.GroepInvoer;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.ProjectService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.stereotype.Component;

@Component
@Slf4j
@AllArgsConstructor
public class CervixOudeNietIngestuurdeZasWriter extends BaseWriter<Client>
{
	private final ProjectService projectService;

	private final HibernateService hibernateService;

	private final LogService logService;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	protected void write(Client client) throws Exception
	{
		var projectGroep = prepareProjectGroep();
		var logEvent = new LogEvent();
		try
		{
			LOG.info("Voeg client (id: " + client.getId() + ") toe aan project groep '" + projectGroep.getNaam() + "'");
			projectService.addClientToProjectGroep(projectGroep, client);
		}
		catch (IllegalStateException e)
		{
			var melding = e.getMessage() + ". Was niet mogelijk om toe te voegen aan project.";
			LOG.warn(melding);
			logEvent.setLevel(Level.WARNING);
			logEvent.setMelding(melding);
		}
		logService.logGebeurtenis(LogGebeurtenis.CERVIX_CLIENT_OUDE_NIET_INGESTUURDE_ZAS_HERINNERING, logEvent, client, Bevolkingsonderzoek.CERVIX);
	}

	private ProjectGroep prepareProjectGroep()
	{
		ProjectGroep projectGroep;
		var context = getExecutionContext();
		if (!context.containsKey(CervixOudeNietIngestuurdeZasConstants.PROJECT_GROEP_ID))
		{
			String groepsNaam = "Populatie van run " + DateUtil.formatShortDate(currentDateSupplier.getDate());
			Long projectId = context.getLong(CervixOudeNietIngestuurdeZasConstants.PROJECT_ID);
			var project = hibernateService.load(Project.class, projectId);
			LOG.info("Maak project groep '" + groepsNaam + "' voor project '" + project.getNaam() + "'");
			projectGroep = new ProjectGroep();
			projectGroep.setProject(project);
			project.getGroepen().add(projectGroep);
			projectGroep.setActief(true);
			projectGroep.setGroepInvoer(GroepInvoer.CRITERIA);
			projectGroep.setActiefDatum(currentDateSupplier.getDate());
			projectGroep.setNaam(groepsNaam);
			projectGroep.setPopulatie(0);
			hibernateService.saveOrUpdate(projectGroep);
			context.put(CervixOudeNietIngestuurdeZasConstants.PROJECT_GROEP_ID, projectGroep.getId());
		}
		else
		{
			projectGroep = hibernateService.load(ProjectGroep.class, context.getLong(CervixOudeNietIngestuurdeZasConstants.PROJECT_GROEP_ID));
		}
		return projectGroep;
	}
}
