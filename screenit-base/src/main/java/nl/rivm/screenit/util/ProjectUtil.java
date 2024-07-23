package nl.rivm.screenit.util;

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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ProjectParameter;
import nl.rivm.screenit.model.ProjectParameterKey;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectBriefActieType;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectStatus;
import nl.rivm.screenit.model.project.ProjectType;

import org.apache.commons.lang.StringUtils;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ProjectUtil
{

	public static Boolean isClientActiefInProject(ProjectClient client, Date date)
	{
		if (client == null)
		{
			return null;
		}
		boolean clientActief = client.getActief();
		boolean groepActief = client.getGroep().getActief();
		boolean projectActief = ProjectStatus.ACTIEF.equals(ProjectUtil.getStatus(client.getProject(), date));
		return clientActief && groepActief && projectActief;
	}

	public static ProjectStatus getStatus(Project project, Date referentieDatum)
	{
		if (referentieDatum.before(project.getStartDatum()))
		{
			return ProjectStatus.NOG_TE_STARTEN;
		}
		else if (!referentieDatum.before(project.getStartDatum()) && referentieDatum.before(project.getEindDatum()))
		{
			return ProjectStatus.ACTIEF;
		}
		return ProjectStatus.BEEINDIGD;
	}

	public static String getClientActiefInProjectString(ProjectClient client, Date date)
	{
		if (client != null)
		{
			String projectType = ProjectType.PROJECT.equals(client.getProject().getType()) ? "project" : "briefproject";
			String projectNaam = " voor " + projectType + ": " + client.getProject().getNaam();
			Boolean isActief = isClientActiefInProject(client, date);
			if (Boolean.TRUE.equals(isActief))
			{
				return "Actief" + projectNaam;
			}
			else if (Boolean.FALSE.equals(isActief))
			{
				if (ProjectStatus.BEEINDIGD.equals(ProjectUtil.getStatus(client.getProject(), date)))
				{
					return "Be&euml;indigd" + projectNaam;
				}
				return "Geselecteerd" + projectNaam;
			}
		}
		return null;
	}

	public static List<ProjectClient> getProjectClientenForBVO(Client client, Bevolkingsonderzoek onderzoek, Date date)
	{
		List<ProjectClient> resultProjectClienten = new ArrayList<>();
		for (ProjectClient projectClient : getHuidigeProjectClienten(client, date, true))
		{
			if (projectClient != null && projectClient.getProject() != null && projectClient.getProject().getBevolkingsonderzoeken() != null
				&& ProjectStatus.ACTIEF.equals(getStatus(projectClient.getProject(), date)))
			{
				for (Bevolkingsonderzoek bvo : projectClient.getProject().getBevolkingsonderzoeken())
				{
					if (onderzoek.equals(bvo))
					{
						resultProjectClienten.add(projectClient);
					}
				}
			}
		}
		return resultProjectClienten;
	}

	public static ProjectClient getHuidigeProjectClient(Client client, Date date)
	{
		return getHuidigeProjectClient(client, date, true);
	}

	public static ProjectClient getHuidigeProjectClient(Client client, Date referentieDatum, boolean groepActiefCheck)
	{
		ProjectClient projectClient = null;
		if (referentieDatum != null && client != null && client.getProjecten() != null)
		{
			for (ProjectClient pClient : client.getProjecten())
			{
				Project project = pClient.getProject();
				if (ProjectType.PROJECT.equals(project.getType()))
				{
					if (pClient.getActief() && (pClient.getGroep().getActief() || !groepActiefCheck)
						&& !ProjectStatus.BEEINDIGD.equals(getStatus(project, referentieDatum)))
					{
						if (projectClient == null || projectClient.getActief() && ProjectStatus.NOG_TE_STARTEN.equals(getStatus(projectClient.getProject(), referentieDatum))
							&& ProjectStatus.ACTIEF.equals(getStatus(project, referentieDatum)))
						{
							projectClient = pClient;
						}
					}
				}
			}
		}
		return projectClient;
	}

	public static List<ProjectClient> getHuidigeProjectClienten(Client client, Date date, boolean groepActiefCheck)
	{
		List<ProjectClient> projectClienten = new ArrayList<>();
		if (date != null && client != null && client.getProjecten() != null)
		{
			for (ProjectClient pClient : client.getProjecten())
			{
				Project project = pClient.getProject();
				if (pClient.getActief() && (pClient.getGroep().getActief() && groepActiefCheck || !groepActiefCheck)
					&& !ProjectStatus.BEEINDIGD.equals(getStatus(project, date)))
				{
					projectClienten.add(pClient);
				}
			}
		}
		return projectClienten;
	}

	public static ProjectBriefActie getProjectBriefActieDefinitie(ProjectClient client, BriefType type)
	{
		if (client != null && client.getProject() != null && type != null)
		{
			Project project = client.getProject();
			for (ProjectBriefActie actie : project.getProjectBriefActies())
			{
				if (ProjectBriefActieType.VERVANGENDEBRIEF.equals(actie.getType()) && actie.getBriefType().equals(type) && actie.getActief())
				{
					return actie;
				}
			}
		}
		return null;
	}

	public static List<BriefType> getBriefTypesWithVervanging(List<ProjectBriefActieType> types)
	{
		List<BriefType> juisteBriefTypes = new ArrayList<BriefType>();
		for (BriefType type : BriefType.values())
		{
			if (types != null && !types.isEmpty() && type.isActief() && type.getBriefActieTypes().containsAll(types))
			{
				juisteBriefTypes.add(type);
			}
		}
		return juisteBriefTypes;
	}

	public static boolean isEinde1eCorrespondentieCheck(Date nu, ProjectClient pClient)
	{
		return !pClient.getBrieven().isEmpty() || pClient.getProject().getEindeInstroom() == null
			|| pClient.getBrieven().isEmpty() && !nu.after(pClient.getProject().getEindeInstroom());
	}

	public static boolean hasParameterSet(Project project, ProjectParameterKey parameterKey)
	{
		if (project != null)
		{
			return project.getParameters().stream().anyMatch(p -> p.getKey().equals(parameterKey) && StringUtils.isNotBlank(p.getValue()));
		}
		return false;
	}

	public static boolean hasParameterSet(ProjectClient projectClient, ProjectParameterKey parameterKey)
	{
		if (projectClient != null)
		{
			return hasParameterSet(projectClient.getProject(), parameterKey);
		}
		return false;
	}

	public static String getParameter(Project project, ProjectParameterKey parameterKey)
	{
		return project.getParameters().stream().filter(p -> p.getKey().equals(parameterKey)).findFirst().orElse(new ProjectParameter()).getValue();
	}
}
