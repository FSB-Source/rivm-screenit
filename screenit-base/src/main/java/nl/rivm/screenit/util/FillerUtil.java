package nl.rivm.screenit.util;

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

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Permissie;
import nl.rivm.screenit.model.Rol;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.colon.enums.ColonTijdSlotType;
import nl.rivm.screenit.model.colon.planning.AfspraakDefinitie;
import nl.rivm.screenit.model.colon.planning.TypeAfspraak;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.model.project.GroepInvoer;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectBestand;
import nl.rivm.screenit.model.project.ProjectBestandType;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.spring.injection.SpringBeanProvider;
import nl.topicuszorg.wicket.planning.model.Discipline;
import nl.topicuszorg.wicket.planning.model.appointment.definition.ActionType;

import org.springframework.util.Assert;

import au.com.bytecode.opencsv.CSVWriter;

import static nl.topicuszorg.hibernate.spring.util.ApplicationContextProvider.getApplicationContext;

@Slf4j
public class FillerUtil
{
	public static UploadDocument getUploadDocumentEnSlaOp(long id, String naam, String contentType, File file, FileStoreLocation fileStoreLocation)
	{
		UploadDocument uploadDocument = new UploadDocument();
		uploadDocument.setNaam(naam);
		uploadDocument.setActief(true);
		uploadDocument.setContentType(contentType);
		uploadDocument.setFile(file);
		try
		{
			UploadDocumentService uploadDocumentService = SpringBeanProvider.getInstance().getBean(UploadDocumentService.class);
			uploadDocumentService.saveOrUpdate(uploadDocument, fileStoreLocation, id, false);
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}
		return uploadDocument;
	}

	public static void addPermissie(Rol rol, Actie actie, Recht recht, ToegangLevel level)
	{
		Assert.isTrue(recht.getActie() == null || recht.getActie().length == 0 || Arrays.asList(recht.getActie()).contains(actie), "Acties");
		Assert.isTrue(recht.getLevel() == null || Arrays.asList(recht.getLevel()).contains(level), "Level");
		Permissie permissie = new Permissie();
		permissie.setActie(actie);
		permissie.setRecht(recht);
		permissie.setToegangLevel(level);
		permissie.setRol(rol);
		rol.getPermissies().add(permissie);
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

		project.getGroepen().add(groep);

		hibernateService.saveOrUpdateAll(groep, project);
		return groep;
	}

	public static void fillProjectWithPopulatie(Project project, int populatie, File file, Iterator<Client> clientenIterator, HibernateService hibernateService)
	{
		ProjectGroep groep = createGroep(project, hibernateService);
		ProjectBestand bestand = maakProjectBestand(project, file, ProjectBestandType.POPULATIE);
		hibernateService.saveOrUpdate(bestand);
		bestand.setGroep(groep);
		groep.getProjectBestanden().add(bestand);

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
		hibernateService.saveOrUpdate(bestand);
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

	public static File createFile(String filePath, List<Client> clienten, List<String> attributen)
	{
		File file = new File(filePath);
		if (file.exists() && !file.delete())
		{
			LOG.error("Verwijderen van bestand " + file.getPath() + " lukt niet");
		}

		try (CSVWriter csvOutput = new CSVWriter(new FileWriter(filePath, true), ','))
		{
			csvOutput.writeNext(getHeaders(attributen));
			for (Client client : clienten)
			{
				csvOutput.writeNext(getAttribuutValuesForEachClient(client, attributen));
			}
		}
		catch (Exception e)
		{
			LOG.error("Error met het bestand creeeren!", e);
		}
		return new File(filePath);
	}

	private static String[] getHeaders(List<String> attributen)
	{
		List<String> headers = new ArrayList<>();
		headers.add("bsn");
		headers.add("geboortedatum");
		for (String attribuut : attributen)
		{
			headers.add(attribuut);
		}
		return headers.toArray(new String[0]);
	}

	private static String[] getAttribuutValuesForEachClient(Client client, List<String> attributen)
	{
		SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy");
		List<String> values = new ArrayList<>();
		values.add(client.getPersoon().getBsn());
		values.add(format.format(client.getPersoon().getGeboortedatum()));
		for (String value : attributen)
		{
			values.add(value);
		}
		return values.toArray(new String[0]);
	}

	public static ProjectBestand maakProjectBestand(Project project, File file, ProjectBestandType bestandType)
	{
		ICurrentDateSupplier currentDateSupplier = getApplicationContext().getBean(ICurrentDateSupplier.class);
		HibernateService hibernateService = getApplicationContext().getBean(HibernateService.class);
		ProjectBestand bestand = new ProjectBestand();
		if (file != null)
		{
			bestand.setUploadDocument(getUploadDocumentEnSlaOp(project.getId(), "voorbeeldBestand.csv", "", file, FileStoreLocation.PROJECT_BESTAND));
		}
		bestand.setUploadDatum(currentDateSupplier.getDate());
		bestand.setProject(project);
		bestand.setStatus(BestandStatus.NOG_TE_VERWERKEN);
		bestand.setStatusDatum(currentDateSupplier.getDate());
		bestand.setAttributen(true);
		bestand.setType(bestandType);
		hibernateService.saveOrUpdate(bestand);
		return bestand;
	}
}
