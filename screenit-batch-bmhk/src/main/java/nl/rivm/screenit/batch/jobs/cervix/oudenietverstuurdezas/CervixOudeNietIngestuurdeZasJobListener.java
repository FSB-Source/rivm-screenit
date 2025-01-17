package nl.rivm.screenit.batch.jobs.cervix.oudenietverstuurdezas;

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

import java.io.IOException;
import java.nio.charset.Charset;
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

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.batch.core.JobExecution;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class CervixOudeNietIngestuurdeZasJobListener extends BaseLogListener
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
		projectNaam = organisatieParameterService.getOrganisatieParameter(null, OrganisatieParameterKey.CERVIX_PROJECT_OUDE_ZAS);
		if (StringUtils.isNotBlank(projectNaam))
		{
			var zoekObject = new Project();
			zoekObject.setNaam(projectNaam);
			zoekObject.setProjectStatussen(List.of(ProjectStatus.ACTIEF));
			zoekObject.setProjectTypes(List.of(ProjectType.BRIEFPROJECT));
			zoekObject.setGroepSelectieType(GroepSelectieType.DYNAMISCH);
			zoekObject.setBevolkingsonderzoeken(List.of(Bevolkingsonderzoek.CERVIX));
			var projecten = projectService.getProjecten(zoekObject, Collections.emptyList(), Collections.emptyList(), -1, -1, new SortState<>("naam", true));
			Project project = null;
			if (!projecten.isEmpty())
			{
				project = projecten.get(0);
			}
			if (project != null)
			{
				jobExecution.getExecutionContext().put(CervixOudeNietIngestuurdeZasConstants.PROJECT_ID, project.getId());
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
		return LogGebeurtenis.CERVIX_OUDE_NIET_INGESTUURDE_ZAS_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_OUDE_NIET_INGESTUURDE_ZAS_AFGEROND;
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
			if (executionContext.containsKey(CervixOudeNietIngestuurdeZasConstants.PROJECT_ID) && StringUtils.isNotBlank(projectNaam))
			{
				melding = "Geen clienten gevonden. Geen groep aangemaakt voor project '" + projectNaam + "'";
				event.setLevel(Level.WARNING);
				if (executionContext.containsKey(CervixOudeNietIngestuurdeZasConstants.PROJECT_GROEP_ID))
				{
					var groepId = executionContext.getLong(CervixOudeNietIngestuurdeZasConstants.PROJECT_GROEP_ID);
					var groep = hibernateService.get(ProjectGroep.class, groepId);
					if (groep != null)
					{
						if (groep.getPopulatie() > 0)
						{
							var projectId = executionContext.getLong(CervixOudeNietIngestuurdeZasConstants.PROJECT_ID);
							var aantalNogTeGaan = getAantalNogTeGaan(projectId);
							melding = groep.getPopulatie() + " client(en) toegevoegd aan project '" + projectNaam + "' in groep '" + groep.getNaam() + "'.";
							melding += " # nog te gaan: " + aantalNogTeGaan;
							event.setLevel(Level.INFO);
						}
					}
					else
					{
						LOG.error("geen entiteit voor groep met id " + groepId + " gevonden");
					}
				}
			}
			else
			{
				melding = "Briefproject '" + StringUtils.defaultIfEmpty(projectNaam, "&lt;leeg&gt;") + "' of niet bekend of niet actief, dynamisch of BMHK.";
				event.setLevel(Level.ERROR);
			}
			event.setMelding(melding);
		}
		return event;
	}

	private String getAantalNogTeGaan(Long projectId)
	{
		try
		{
			var queryString = IOUtils.toString(getClass().getResourceAsStream("step/CervixOudeNietIngestuurdeZasReader.sql"), Charset.defaultCharset());
			queryString = queryString
				.replace("select c.id clientId", "select count(c.id)")
				.replace("order by a.verstuurd_datum", "");
			var query = hibernateService.getHibernateSession().createSQLQuery(queryString);
			query.setParameter("projectId", projectId);

			return query.setReadOnly(true).uniqueResult().toString();
		}
		catch (IOException e)
		{
			LOG.error("Fout bij lezen van sql bestand: ", e);
			return "niet te bepalen (informeer bij Topicus)";
		}
	}
}
