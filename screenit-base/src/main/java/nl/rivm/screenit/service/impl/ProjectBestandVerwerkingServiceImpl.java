package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.dao.ProjectDao;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectAttribuut;
import nl.rivm.screenit.model.project.ProjectBestand;
import nl.rivm.screenit.model.project.ProjectBestandType;
import nl.rivm.screenit.model.project.ProjectBestandVerwerking;
import nl.rivm.screenit.model.project.ProjectBestandVerwerkingEntry;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectClientAttribuut;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.rivm.screenit.model.project.ProjectInactiefReden;
import nl.rivm.screenit.model.project.ProjectType;
import nl.rivm.screenit.service.ClientDoelgroepService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.ProjectBestandVerwerkingService;
import nl.rivm.screenit.service.ProjectService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.util.collections.CollectionUtils;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import ca.uhn.hl7v2.util.StringUtil;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class ProjectBestandVerwerkingServiceImpl implements ProjectBestandVerwerkingService
{
	private static final Logger LOG = LoggerFactory.getLogger(ProjectBestandVerwerkingServiceImpl.class);

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ClientService clientService;

	@Autowired
	private ProjectDao projectDao;

	@Autowired
	private ClientDoelgroepService doelgroepService;

	@Autowired
	private ProjectService projectService;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void voorbereidingVoorVerwerking(ProjectBestandVerwerkingContext context, ProjectBestand bestand)
	{
		if (!CollectionUtils.isEmpty(context.getOnbekendeAttributen()))
		{
			for (String onbekendeHeader : context.getOnbekendeAttributen())
			{
				addBestandsMelding(bestand, null, "Er is geen attribuut gevonden met naam " + onbekendeHeader);
			}
		}
		if (context.getVolgordeHeaders().size() == 0 && bestand.isAttributen())
		{
			throw new IllegalStateException("Er zijn geen attributen gevonden in het bestand");
		}

		if (bestand.getVerwerking() != null)
		{

			ProjectBestandVerwerking verwerking = bestand.getVerwerking();
			verwerking.setAttributenGevonden(context.getAttributenGevondenString());
			hibernateService.saveOrUpdate(verwerking);
		}

		setBestandStatus(bestand, BestandStatus.BEZIG_MET_VERWERKEN);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void setBestandStatus(ProjectBestand bestand, BestandStatus status)
	{
		setBestandStatus(bestand, status, null);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
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
	public void verwerkRegel(ProjectBestandVerwerkingContext context)
	{

		try
		{
			SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy");
			ProjectBestand bestand = context.getBestand();
			String bsn = context.getBsnVanHuidigeRegel();
			Date geboortedatum = context.getGeboortedatumVanHuidigeRegel();
			Client client = clientService.getClientByBsn(context.getBsnVanHuidigeRegel());
			if (client == null)
			{
				throw new IllegalStateException("Geen client gevonden met bsn: " + bsn + " en geboortedatum: " + format.format(geboortedatum));
			}
			if (!geboortedatum.equals(client.getPersoon().getGeboortedatum()))
			{
				throw new IllegalStateException("Geen client gevonden met bsn: " + bsn + " en geboortedatum: " + format.format(geboortedatum));
			}

			ProjectClient projectClient = projectDao.getProjectClient(client, bestand);
			if (projectClient == null && ProjectBestandType.POPULATIE == bestand.getType() && bestand.getGroep() != null)
			{
				projectClient = verwerkPopulatie(context, client);
				if (bestand.isAttributen())
				{
					verwerkAttributen(context, projectClient);
				}
			}
			if (projectClient == null)
			{
				throw new IllegalStateException("Client met bsn " + bsn + " behoort niet tot de projectpopulatie");
			}
			if (bestand.getGroep() != null && !bestand.getGroep().equals(projectClient.getGroep()))
			{
				throw new IllegalStateException("Client met bsn " + bsn + " behoort tot een andere groep van de projectpopulatie");
			}

			switch (bestand.getType())
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

			if (bestand.isAttributen()) 
			{
				verwerkAttributen(context, projectClient);
			}

			ProjectBestandVerwerking verwerking = context.getBestand().getVerwerking();
			verwerking.setRegelsVerwerkt(verwerking.getRegelsVerwerkt() + 1);
			hibernateService.saveOrUpdate(verwerking);
		}
		catch (IllegalStateException e)
		{
			addBestandsMelding(context.getBestand(), context.getRegelnummer(), e.getMessage());
			ProjectBestandVerwerking verwerking = context.getBestand().getVerwerking();
			verwerking.setRegelsMislukt(verwerking.getRegelsMislukt() + 1);
			hibernateService.saveOrUpdate(verwerking);
			LOG.warn("Probleem opgetreden met verwerken van project bestand " + context.getBestand().getUploadDocument().getNaam() + " op regelnummer " + context.getRegelnummer());
		}
	}

	private void verwerkAttributen(ProjectBestandVerwerkingContext context, ProjectClient projectClient) throws IllegalStateException
	{
		for (Map.Entry<ProjectAttribuut, Integer> attribuut : context.getVolgordeHeaders().entrySet())
		{
			String csvWaarde = context.getHuidigeRegel().get(attribuut.getValue());
			if (StringUtil.isBlank(csvWaarde))
			{
				throw new IllegalStateException("Geen waarde gevonden van attribuut " + attribuut.getKey().getNaam());
			}

			ProjectClientAttribuut attribuutVanClient = projectDao.getProjectClientAttribuut(projectClient, attribuut.getKey());
			if (attribuutVanClient == null)
			{
				attribuutVanClient = new ProjectClientAttribuut();
				attribuutVanClient.setProjectClient(projectClient);
				attribuutVanClient.setAttribute(attribuut.getKey());
				projectClient.getAttributen().add(attribuutVanClient);
			}
			attribuutVanClient.setValue(csvWaarde);
			hibernateService.saveOrUpdateAll(attribuutVanClient, projectClient);
		}

		ProjectBestandVerwerking verwerking = context.getBestand().getVerwerking();
		verwerking.setAttributenGewijzigd(verwerking.getAttributenGewijzigd() + 1);
		hibernateService.saveOrUpdate(verwerking);
	}

	private ProjectClient verwerkPopulatie(ProjectBestandVerwerkingContext context, Client client) throws IllegalStateException
	{
		ProjectBestand bestand = context.getBestand();
		ProjectGroep groep = bestand.getGroep();
		try
		{
			return projectService.addClientToProjectGroep(groep, client);
		}
		catch (IllegalStateException e)
		{
			throw new IllegalStateException(e.getMessage() + ". Verwijder deze cliënt uit de csv.");
		}
	}

	private void verwerkInactiverenProjectClient(ProjectBestandVerwerkingContext context, ProjectClient projectClient)
	{
		if (Boolean.TRUE.equals(projectClient.getActief()))
		{
			projectClient.setProjectInactiefDatum(currentDateSupplier.getDate());
			projectClient.setProjectInactiefReden(ProjectInactiefReden.INACTIVATIE_DOOR_UPLOAD);
			projectClient.setActief(false);
			hibernateService.saveOrUpdate(projectClient);

			ProjectBestandVerwerking verwerking = context.getBestand().getVerwerking();
			verwerking.setGeinactiveerd(verwerking.getGeinactiveerd() + 1);
			hibernateService.saveOrUpdate(verwerking);
		}
		else
		{
			throw new IllegalStateException("Deze project client is al inactief en kan daarom niet worden geinactiveerd.");
		}
	}

	private void verwerkHeractiverenProjectClient(ProjectBestandVerwerkingContext context, ProjectClient projectClient)
	{
		if (Boolean.FALSE.equals(projectClient.getActief()))
		{
			projectClient.setProjectInactiefDatum(null);
			projectClient.setProjectInactiefReden(null);
			projectClient.setActief(true);
			hibernateService.saveOrUpdate(projectClient);

			ProjectBestandVerwerking verwerking = context.getBestand().getVerwerking();
			verwerking.setGeheractiveerd(verwerking.getGeheractiveerd() + 1);
			hibernateService.saveOrUpdate(verwerking);
		}
		else
		{
			throw new IllegalStateException("Deze project client is al actief en kan daarom niet worden geheractiveerd.");
		}
	}

	private void verwerkVerwijderenProjectClient(ProjectBestandVerwerkingContext context, ProjectClient projectClient)
	{
		ColonDossier colonDossier = projectClient.getClient().getColonDossier();
		CervixDossier cervixDossier = projectClient.getClient().getCervixDossier();
		MammaDossier mammaDossier = projectClient.getClient().getMammaDossier();
		Project project = projectClient.getProject();
		ProjectGroep projectGroep = projectClient.getGroep();

		List<Bevolkingsonderzoek> bevolkingsonderzoeken = project.getBevolkingsonderzoeken();
		boolean briefProject = ProjectType.BRIEFPROJECT.equals(project.getType());
		boolean projectZonderBvo = CollectionUtils.isEmpty(bevolkingsonderzoeken);
		boolean actieveColonRonde = colonDossier.getLaatsteScreeningRonde() != null
			&& rondeMetMogelijkeActiviteitInProject(colonDossier.getLaatsteScreeningRonde(), project);
		boolean actieveCervixRonde = cervixDossier != null && cervixDossier.getLaatsteScreeningRonde() != null
			&& rondeMetMogelijkeActiviteitInProject(cervixDossier.getLaatsteScreeningRonde(), project);
		boolean actieveMammaRonde = mammaDossier != null && mammaDossier.getLaatsteScreeningRonde() != null
			&& rondeMetMogelijkeActiviteitInProject(mammaDossier.getLaatsteScreeningRonde(), project);
		boolean bvoMetActieveRonde = false;
		String bvos = "";

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

			ProjectBestandVerwerking verwerking = context.getBestand().getVerwerking();
			verwerking.setVerwijderd(verwerking.getVerwijderd() + 1);
			hibernateService.saveOrUpdate(verwerking);
		}
	}

	private boolean rondeMetMogelijkeActiviteitInProject(ScreeningRonde ronde, Project project)
	{
		return currentDateSupplier.getDate().after(project.getStartDatum()) && ScreeningRondeStatus.LOPEND.equals(ronde.getStatus())
			|| ScreeningRondeStatus.AFGEROND.equals(ronde.getStatus()) && DateUtil.compareAfter(ronde.getStatusDatum(), project.getStartDatum());
	}

}
