package nl.rivm.screenit.batch.jobs.cervix.verlatedeelnamecovid;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import java.util.Collections;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.helpers.BaseLogListener;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.project.GroepSelectieType;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.rivm.screenit.model.project.ProjectStatus;
import nl.rivm.screenit.model.project.ProjectType;
import nl.rivm.screenit.service.BaseProjectService;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.batch.core.JobExecution;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class CervixVerlateDeelnameCovidJobListener extends BaseLogListener
{
	@Autowired
	private OrganisatieParameterService organisatieParameterService;

	@Autowired
	private BaseProjectService projectService;

	@Autowired
	private HibernateService hibernateService;

	private String projectNaam;

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return Bevolkingsonderzoek.CERVIX;
	}

	@Override
	protected void beforeStarting(JobExecution jobExecution)
	{
		super.beforeStarting(jobExecution);
		projectNaam = organisatieParameterService.getOrganisatieParameter(null, OrganisatieParameterKey.CERVIX_PROJECT_VERLATE_DEELNAME);
		if (StringUtils.isNotBlank(projectNaam))
		{
			var zoekObject = new Project();
			zoekObject.setNaam(projectNaam);
			zoekObject.setProjectStatussen(List.of(ProjectStatus.ACTIEF));
			zoekObject.setProjectTypes(List.of(ProjectType.PROJECT));
			zoekObject.setGroepSelectieType(GroepSelectieType.DYNAMISCH);
			zoekObject.setBevolkingsonderzoeken(List.of(Bevolkingsonderzoek.CERVIX));
			List<Project> projecten = projectService.getProjecten(zoekObject, Collections.emptyList(), Collections.emptyList(), -1, -1, new SortState<>("naam", true));
			Project project = null;
			if (!projecten.isEmpty())
			{
				project = projecten.get(0);
			}
			if (project != null)
			{
				jobExecution.getExecutionContext().put(CervixVerlateDeelnameCovidConstants.PROJECT_ID, project.getId());
			}
		}
	}

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_VERLATE_DEELNAME_COVID_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_VERLATE_DEELNAME_COVID_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		var event = super.eindLogging(jobExecution);
		if (event.getLevel() == Level.INFO)
		{
			String melding = "";
			var executionContext = jobExecution.getExecutionContext();
			if (executionContext.containsKey(CervixVerlateDeelnameCovidConstants.PROJECT_ID) && StringUtils.isNotBlank(projectNaam))
			{
				melding = "Geen clienten gevonden. Geen herdrukbrieven aangemaakt voor project '" + projectNaam + "'";
				event.setLevel(Level.WARNING);
				if (executionContext.containsKey(CervixVerlateDeelnameCovidConstants.PROJECT_GROEP_ID))
				{
					long groepId = executionContext.getLong(CervixVerlateDeelnameCovidConstants.PROJECT_GROEP_ID);
					ProjectGroep groep = hibernateService.get(ProjectGroep.class, groepId);
					if (groep != null)
					{
						if (groep.getPopulatie() > 0)
						{
							melding = groep.getPopulatie() + " herdrukbrieven aangemaakt binnen project '" + projectNaam + "' in groep '" + groep.getNaam() + "'";
							event.setLevel(Level.INFO);
						}
						groep.setActief(false);
						hibernateService.saveOrUpdate(groep);
					}
					else
					{
						LOG.error("geen entiteit voor groep met id " + groepId + " gevonden");
					}
				}
			}
			else
			{
				melding = "Project '" + StringUtils.defaultIfEmpty(projectNaam, "&lt;leeg&gt;") + "' of niet bekend of niet actief, dynamisch of BMHK.";
				event.setLevel(Level.ERROR);
			}
			event.setMelding(melding);
		}
		else if (event.getLevel() == Level.ERROR)
		{
			var unsupportedException = jobExecution.getAllFailureExceptions().stream().filter(UnsupportedOperationException.class::isInstance).findFirst();
			unsupportedException.ifPresent(throwable -> event.setMelding(throwable.getMessage()));
		}
		return event;
	}
}
