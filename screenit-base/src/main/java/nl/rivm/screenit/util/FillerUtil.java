package nl.rivm.screenit.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Permissie;
import nl.rivm.screenit.model.Rol;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.colon.enums.ColonTijdSlotType;
import nl.rivm.screenit.model.colon.planning.AfspraakDefinitie;
import nl.rivm.screenit.model.colon.planning.TypeAfspraak;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.model.project.GroepInvoer;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.rivm.screenit.model.project.ProjectImport;
import nl.rivm.screenit.service.FileService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.planning.model.Discipline;
import nl.topicuszorg.wicket.planning.model.appointment.definition.ActionType;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.Assert;

import static nl.topicuszorg.hibernate.spring.util.ApplicationContextProvider.getApplicationContext;

public class FillerUtil
{
	private static final Logger LOG = LoggerFactory.getLogger(FillerUtil.class);

	public static UploadDocument getUploadDocumentEnSlaOp(long id, String naam, String contentType, File file, FileStoreLocation fileStoreLocation)
	{
		UploadDocument uploadDocument = new UploadDocument();
		uploadDocument.setNaam(naam);
		uploadDocument.setActief(true);
		uploadDocument.setContentType(contentType);
		uploadDocument.setFile(file);
		try
		{
			FileService fileService = getApplicationContext().getBean(FileService.class);
			fileService.saveOrUpdateUploadDocument(uploadDocument, fileStoreLocation, id, false);
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}
		return uploadDocument;
	}

	public static void addPermissie(Rol rol, Actie actie, Recht recht, ToegangLevel level)
	{
		Permissie permissie = new Permissie();
		permissie.setActie(actie);
		permissie.setRecht(recht);
		permissie.setToegangLevel(level);
		permissie.setRol(rol);
		rol.getPermissies().add(permissie);
	}

	public static void addPermissieGebruiker(Rol rol, Actie actie, Recht recht, ToegangLevel level)
	{
		Assert.isTrue(recht.getActie() == null || recht.getActie().length == 0 || Arrays.asList(recht.getActie()).contains(actie));
		Assert.isTrue(recht.getLevel() == null || Arrays.asList(recht.getLevel()).contains(level));
		addPermissie(rol, actie, recht, level);
	}

	private static ProjectGroep createGroep(Project project, HibernateService hibernateService)
	{
		String projectNaam = project.getNaam();

		ProjectGroep groep = new ProjectGroep();
		groep.setNaam(projectNaam + " - Groep 1");
		groep.setActief(Boolean.TRUE);
		groep.setActiefDatum(new Date());
		groep.setProject(project);
		groep.setGroepInvoer(GroepInvoer.IMPORT);

		ProjectImport pImport = new ProjectImport();
		pImport.setGroep(groep);
		pImport.setSkipFouten(true);

		groep.setProjectImport(pImport);

		if (project.getGroepen() == null)
		{
			project.setGroepen(new ArrayList<>());
		}

		project.getGroepen().add(groep);

		if (groep.getClienten() == null)
		{
			groep.setClienten(new ArrayList<>());
		}
		hibernateService.saveOrUpdateAll(groep, project);
		return groep;
	}

	public static void fillProjectWithPopulatie(Project project, int populatie, Iterator<Client> clientenIterator, HibernateService hibernateService)
	{
		ProjectGroep groep = FillerUtil.createGroep(project, hibernateService);
		groep.setPopulatie(populatie);

		for (int i = 0; i < populatie; i++)
		{
			Client client = clientenIterator.next();

			ProjectClient pClient = new ProjectClient();
			pClient.setClient(client);
			pClient.setGroep(groep);
			pClient.setProject(project);
			pClient.setToegevoegd(new Date());
			pClient.setActief(Boolean.TRUE);

			groep.getClienten().add(pClient);

			hibernateService.saveOrUpdate(pClient);
		}
		LOG.debug("Voor project " + project.getNaam() + " zijn er " + populatie + " clienten aangemaakt!");
		hibernateService.saveOrUpdate(groep);
	}

	public static void definieerAfspraakDefinitie(AfspraakDefinitie definitie, Discipline discipline)
	{
		definitie.setActief(true);
		definitie.setDuurAfspraakInMinuten(Integer.valueOf(15));
		definitie.setLabel(ColonTijdSlotType.ROOSTER_ITEM.getTitle());
		definitie.setType(ActionType.APPOINTMENT);
		definitie.setTypeAfspraak(TypeAfspraak.AFSPRAAK);
		definitie.setPossibleLocations(new ArrayList<>());
		definitie.addDiscipline(discipline);
	}

	public static String stripComments(List<String> list)
	{
		StringBuffer buffer = new StringBuffer();
		for (String line : list)
		{
			if (!line.startsWith("//") && !line.startsWith("--"))
			{
				buffer.append(line + "\n");
			}
		}
		return buffer.toString();
	}
}
