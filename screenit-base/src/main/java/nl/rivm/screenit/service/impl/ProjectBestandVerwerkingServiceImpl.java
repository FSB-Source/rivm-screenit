package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectBestand;
import nl.rivm.screenit.model.project.ProjectBestandType;
import nl.rivm.screenit.model.project.ProjectBestandVerwerking;
import nl.rivm.screenit.model.project.ProjectBestandVerwerkingEntry;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectClientAttribuut;
import nl.rivm.screenit.model.project.ProjectInactiefReden;
import nl.rivm.screenit.model.project.ProjectType;
import nl.rivm.screenit.service.BaseProjectService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.ProjectBestandVerwerkingService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.util.collections.CollectionUtils;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Slf4j
public class ProjectBestandVerwerkingServiceImpl implements ProjectBestandVerwerkingService
{
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private BaseProjectService projectService;

	@Override
	@Transactional
	public void voorbereidingVoorVerwerking(BaseProjectBestandVerwerkingContext context, ProjectBestand bestand)
	{
		if (bestand.getVerwerking() != null)
		{
			var verwerking = bestand.getVerwerking();
			verwerking.setAttributenGevonden(context.getAttributenGevondenString());
			hibernateService.saveOrUpdate(verwerking);
		}

		setBestandStatus(bestand, BestandStatus.BEZIG_MET_VERWERKEN);
	}

	@Override
	@Transactional
	public void setBestandStatus(ProjectBestand bestand, BestandStatus status)
	{
		setBestandStatus(bestand, status, null);
	}

	@Override
	@Transactional
	public void setBestandStatus(ProjectBestand bestand, BestandStatus status, String melding)
	{
		bestand.setStatus(status);
		bestand.setStatusDatum(currentDateSupplier.getDate());
		hibernateService.saveOrUpdate(bestand);

		if (melding != null)
		{
			addBestandsMelding(bestand, null, melding);
		}
	}

	private void addBestandsMelding(ProjectBestand bestand, Integer regelnummer, String melding)
	{
		ProjectBestandVerwerking verwerking = bestand.getVerwerking();
		ProjectBestandVerwerkingEntry entry = new ProjectBestandVerwerkingEntry();
		entry.setRegelNummer(regelnummer);
		entry.setMelding(melding);
		entry.setVerwerking(verwerking);
		verwerking.getMeldingen().add(entry);
		hibernateService.saveOrUpdateAll(verwerking, entry);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwerkRegel(BaseProjectBestandVerwerkingContext context)
	{

		try
		{
			var client = context.getClient();

			var projectClient = projectService.getProjectClient(client, context.getProject());
			var groep = context.getGroep();
			if (projectClient == null && ProjectBestandType.POPULATIE == context.getType() && groep != null)
			{
				projectClient = verwerkPopulatie(context, client);
				if (context.heeftAttributen())
				{
					verwerkAttributen(context, projectClient);
				}
			}
			if (projectClient == null)
			{
				throw new IllegalStateException("Client behoort niet tot de projectpopulatie");
			}
			if (groep != null && !groep.equals(projectClient.getGroep()))
			{
				throw new IllegalStateException("Client behoort tot een andere groep van de projectpopulatie");
			}

			switch (context.getType())
			{
			case INACTIVEREN:
				verwerkInactiverenProjectClient(context, projectClient);
				break;
			case HERACTIVEREN:
				verwerkHeractiverenProjectClient(context, projectClient);
				break;
			case VERWIJDEREN:
				verwerkVerwijderenProjectClient(context, projectClient);
				break;
			}

			if (context.heeftAttributen()) 
			{
				verwerkAttributen(context, projectClient);
			}

			context.verwerkingGeslaagd();
		}
		catch (IllegalStateException e)
		{
			context.verwerkingMislukt(e);
		}
	}

	private void verwerkAttributen(BaseProjectBestandVerwerkingContext context, ProjectClient projectClient) throws IllegalStateException
	{
		for (var attribuut : context.getAttributen())
		{
			var waarde = context.getAttribuutWaarde(attribuut);
			var attribuutVanClient = projectService.getProjectClientAttribuut(projectClient, attribuut);
			if (attribuutVanClient == null)
			{
				attribuutVanClient = new ProjectClientAttribuut();
				attribuutVanClient.setProjectClient(projectClient);
				attribuutVanClient.setAttribuut(attribuut);
				projectClient.getAttributen().add(attribuutVanClient);
			}
			attribuutVanClient.setValue(waarde);
			hibernateService.saveOrUpdateAll(attribuutVanClient, projectClient);
		}

		context.attribuutGewijzigd();
	}

	private ProjectClient verwerkPopulatie(BaseProjectBestandVerwerkingContext context, Client client) throws IllegalStateException
	{
		var groep = context.getGroep();
		try
		{
			return projectService.addClientToProjectGroep(groep, client);
		}
		catch (IllegalStateException e)
		{
			throw new IllegalStateException(e.getMessage() + ". Verwijder deze cliënt uit de csv.");
		}
	}

	private void verwerkInactiverenProjectClient(BaseProjectBestandVerwerkingContext context, ProjectClient projectClient)
	{
		if (Boolean.TRUE.equals(projectClient.getActief()))
		{
			projectClient.setProjectInactiefDatum(currentDateSupplier.getDate());
			projectClient.setProjectInactiefReden(ProjectInactiefReden.INACTIVATIE_DOOR_UPLOAD);
			projectClient.setActief(false);
			hibernateService.saveOrUpdate(projectClient);

			context.clientGeinactiveerd();

		}
		else
		{
			throw new IllegalStateException("Deze project client is al inactief en kan daarom niet worden geinactiveerd.");
		}
	}

	private void verwerkHeractiverenProjectClient(BaseProjectBestandVerwerkingContext context, ProjectClient projectClient)
	{
		if (Boolean.FALSE.equals(projectClient.getActief()))
		{
			projectClient.setProjectInactiefDatum(null);
			projectClient.setProjectInactiefReden(null);
			projectClient.setActief(true);
			hibernateService.saveOrUpdate(projectClient);

			context.clientGeheractiveerd();
		}
		else
		{
			throw new IllegalStateException("Deze project client is al actief en kan daarom niet worden geheractiveerd.");
		}
	}

	private void verwerkVerwijderenProjectClient(BaseProjectBestandVerwerkingContext context, ProjectClient projectClient)
	{
		var colonDossier = projectClient.getClient().getColonDossier();
		var cervixDossier = projectClient.getClient().getCervixDossier();
		var mammaDossier = projectClient.getClient().getMammaDossier();
		var project = projectClient.getProject();
		var projectGroep = projectClient.getGroep();

		var bevolkingsonderzoeken = project.getBevolkingsonderzoeken();
		var briefProject = ProjectType.BRIEFPROJECT.equals(project.getType());
		var projectZonderBvo = CollectionUtils.isEmpty(bevolkingsonderzoeken);
		var actieveColonRonde = colonDossier.getLaatsteScreeningRonde() != null
			&& rondeMetMogelijkeActiviteitInProject(colonDossier.getLaatsteScreeningRonde(), project);
		var actieveCervixRonde = cervixDossier != null && cervixDossier.getLaatsteScreeningRonde() != null
			&& rondeMetMogelijkeActiviteitInProject(cervixDossier.getLaatsteScreeningRonde(), project);
		var actieveMammaRonde = mammaDossier != null && mammaDossier.getLaatsteScreeningRonde() != null
			&& rondeMetMogelijkeActiviteitInProject(mammaDossier.getLaatsteScreeningRonde(), project);
		var bvoMetActieveRonde = false;
		var bvos = "";

		if (projectZonderBvo && (actieveCervixRonde || actieveColonRonde))
		{
			bvoMetActieveRonde = true;
			bvos += "DK/BMHK";
		}
		else
		{
			if (bevolkingsonderzoeken.contains(Bevolkingsonderzoek.COLON) && actieveColonRonde)
			{
				bvoMetActieveRonde = true;
				bvos += "DK";
			}
			if (bevolkingsonderzoeken.contains(Bevolkingsonderzoek.CERVIX) && actieveCervixRonde)
			{
				bvoMetActieveRonde = true;
				if (StringUtils.isEmpty(bvos))
				{
					bvos += "BMHK";
				}
				else
				{
					bvos += "/BMHK";
				}
			}
			if (bevolkingsonderzoeken.contains(Bevolkingsonderzoek.MAMMA) && actieveMammaRonde)
			{
				bvoMetActieveRonde = true;
				if (StringUtils.isEmpty(bvos))
				{
					bvos += "BK";
				}
				else
				{
					bvos += "/BK";
				}
			}
		}

		if (bvoMetActieveRonde && !briefProject)
		{
			throw new IllegalStateException("Deze project client heeft een open ronde " + bvos + " tijdens het project (gehad) en kan daarom niet worden verwijderd.");
		}
		else
		{
			project.getClienten().remove(projectClient);
			hibernateService.saveOrUpdate(project);

			projectGroep.getClienten().remove(projectClient);
			projectGroep.setPopulatie(projectGroep.getPopulatie() - 1);
			hibernateService.saveOrUpdate(projectGroep);

			hibernateService.delete(projectClient);

			context.clientVerwijderd();
		}
	}

	private boolean rondeMetMogelijkeActiviteitInProject(ScreeningRonde ronde, Project project)
	{
		return currentDateSupplier.getDate().after(project.getStartDatum()) && ScreeningRondeStatus.LOPEND.equals(ronde.getStatus())
			|| ScreeningRondeStatus.AFGEROND.equals(ronde.getStatus()) && DateUtil.compareAfter(ronde.getStatusDatum(), project.getStartDatum());
	}

}
