package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.io.FileReader;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import nl.rivm.screenit.dao.ProjectDao;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.model.formulieren.ScreenitFormulierInstantie;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectAttribuut;
import nl.rivm.screenit.model.project.ProjectBestand;
import nl.rivm.screenit.model.project.ProjectBestandType;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.rivm.screenit.model.project.ProjectImport;
import nl.rivm.screenit.model.project.ProjectImportMelding;
import nl.rivm.screenit.model.project.ProjectInactiefReden;
import nl.rivm.screenit.model.project.ProjectInactiveerDocument;
import nl.rivm.screenit.model.project.ProjectType;
import nl.rivm.screenit.model.project.ProjectVragenlijst;
import nl.rivm.screenit.model.project.ProjectVragenlijstAntwoordenHolder;
import nl.rivm.screenit.model.project.ProjectVragenlijstStatus;
import nl.rivm.screenit.model.project.ProjectVragenlijstUitzettenVia;
import nl.rivm.screenit.model.vragenlijsten.VragenlijstAntwoorden;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.ProjectService;
import nl.rivm.screenit.util.AfmeldingUtil;
import nl.rivm.screenit.util.BezwaarUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.formulieren2.persistence.resultaat.FormulierResultaatImpl;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.aspose.words.Document;
import com.aspose.words.ImportFormatMode;

import au.com.bytecode.opencsv.CSVReader;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class ProjectServiceImpl implements ProjectService
{
	private static final Logger LOG = LoggerFactory.getLogger(ProjectServiceImpl.class);

	private static final String VRAGENLIJST_PREFIX = "vragenlijst/";

	private ExecutorService executerService;

	@Autowired
	private AutorisatieService autorisatieService;

	@Autowired
	private ProjectDao projectDao;

	@Autowired
	private ClientService clientService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private FileService fileService;

	@Autowired
	private AsposeService asposeService;

	@Autowired
	private String applicationUrl;

	public ProjectServiceImpl()
	{
		executerService = Executors.newSingleThreadExecutor();
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdateProject(Project project, InstellingGebruiker instellingGebruiker)
	{
		if (project.getId() == null)
		{
			if (project.getType().equals(ProjectType.BRIEFPROJECT))
			{
				String melding = "Briefprojectnaam: " + project.getNaam();
				logService.logGebeurtenis(LogGebeurtenis.BRIEFPROJECT_AANGEMAAKT, instellingGebruiker, melding);
			}
			else
			{
				String melding = "Projectnaam: " + project.getNaam();
				logService.logGebeurtenis(LogGebeurtenis.PROJECT_AANGEMAAKT, instellingGebruiker, melding);
			}
		}
		else
		{
			if (project.getType().equals(ProjectType.BRIEFPROJECT))
			{
				String melding = "Briefprojectnaam: " + project.getNaam();
				logService.logGebeurtenis(LogGebeurtenis.BRIEFPROJECT_GEWIJZIGD, instellingGebruiker, melding);
			}
			else
			{
				String melding = "Projectnaam: " + project.getNaam();
				logService.logGebeurtenis(LogGebeurtenis.PROJECT_GEWIJZIGD, instellingGebruiker, melding);
			}
		}

		hibernateService.saveOrUpdate(project);
	}

	@Override
	public List<Project> getProjecten(Project zoekObject, List<Long> instellingIdsProject, List<Long> instellingIdsBriefproject, long first, long count,
		SortState<String> sortState)
	{
		return projectDao.getProjecten(zoekObject, instellingIdsProject, instellingIdsBriefproject, first, count, sortState);
	}

	@Override
	public long getCountProjecten(Project zoekObject, List<Long> instellingIdsProject, List<Long> instellingIdsBriefproject)
	{
		return projectDao.getCountProjecten(zoekObject, instellingIdsProject, instellingIdsBriefproject);
	}

	@Override
	public Iterator<ProjectGroep> getGroepen(ProjectGroep zoekObject, long first, long count, SortState<String> sortState)
	{
		return projectDao.getGroepen(zoekObject, first, count, sortState);
	}

	@Override
	public long getCountGroepen(ProjectGroep zoekObject)
	{
		return projectDao.getCountGroepen(zoekObject);
	}

	@Override
	public Iterator<ProjectClient> getClientProjecten(ProjectClient client, long first, long count, SortState<String> sortState)
	{
		return projectDao.getClientProjecten(client, first, count, sortState);
	}

	@Override
	public long getCountClientProjecten(ProjectClient client)
	{
		return projectDao.getCountClientProjecten(client);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public Map<ProjectImportMelding, List<String>> verwerkXLSNaarProjectClienten(ProjectGroep groep, InstellingGebruiker ingelogdeGebruiker, UploadDocument upload,
		Boolean skipFouten)
	{

		boolean error = false;
		SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy");
		List<ProjectClient> projectClienten = new ArrayList<>();
		List<Client> clienten = new ArrayList<>();
		Map<ProjectImportMelding, List<String>> meldingen = new HashMap<>();

		int regelnummer = 0;
		int populatie = 0;
		int foutClienten = 0;
		CSVReader csvReader = null;
		try
		{
			fileService.saveOrUpdateUploadDocument(upload, FileStoreLocation.PROJECT_IMPORT, groep.getProject().getId());

			csvReader = new CSVReader(new FileReader(fileService.load(upload)), ';');

			for (String[] line : csvReader.readAll())
			{
				regelnummer++;

				if (line.length < 2)
				{
					error = true;
					putMeldingInHash(meldingen, ProjectImportMelding.ERROR,
						"Bsn en geboortedatum is verplicht. Wanneer deze aanwezig zijn controleer op scheidingsteken tussen velden op een ;");
					return meldingen;
				}

				String bsn = line[0];
				if (!StringUtils.isNotBlank(bsn) || !StringUtils.isNumeric(bsn))
				{

					continue;
				}

				if (bsn.length() < 9)
				{
					bsn = StringUtils.leftPad(bsn, 9, '0');
				}

				String datum = line[1];
				Date geboortedatum = null;
				try
				{
					geboortedatum = format.parse(datum);
				}
				catch (ParseException e)
				{
					String melding = "Regel " + regelnummer + ": Verkeerde formaat datum aangeleverder, verwacht: dag-maand-jaar. (Voorbeeld. 12-01-1950)";
					LOG.error(melding);
					putMeldingInHash(meldingen, ProjectImportMelding.ERROR, melding);
					error = true;
					continue;
				}

				Client client = clientService.getClientByBsn(bsn);
				List<String> nieuweMeldingen = valideerImportOpFoutenClient(groep, client, geboortedatum, regelnummer);

				if (nieuweMeldingen.isEmpty())
				{
					putMeldingInHash(meldingen, ProjectImportMelding.SUCCES,
						"Regel " + regelnummer + ": Cliënt met BSN " + bsn + " is succesvol toegevoegd aan de project populatie.");
					ProjectClient projectClient = new ProjectClient(client, groep);
					projectClient.setToegevoegd(currentDateSupplier.getDate());
					projectClienten.add(projectClient);
					client.getProjecten().add(projectClient);
					clienten.add(client);
					populatie++;
				}
				else
				{
					error = true;
					putAllMeldingInHash(meldingen, nieuweMeldingen);
					foutClienten++;
				}

			}

			if (!error || skipFouten)
			{
				ProjectImport projectImport = new ProjectImport();
				projectImport.setUploadDocument(upload);
				projectImport.setGroep(groep);
				projectImport.setSkipFouten(skipFouten);

				groep.setPopulatie(populatie);
				groep.setProjectImport(projectImport);
				groep.setClienten(projectClienten);
				hibernateService.saveOrUpdate(groep);
				hibernateService.saveOrUpdateAll(projectClienten);
				hibernateService.saveOrUpdate(projectImport);
				hibernateService.saveOrUpdateAll(clienten);

				putMeldingInHash(meldingen, ProjectImportMelding.MELDING,
					"Groep " + groep.getNaam() + "  toegevoegd: " + projectClienten.size() + " geselecteerd, aantal overgeslagen: " + foutClienten + ".");
			}

			if (groep.getProject().getType().equals(ProjectType.BRIEFPROJECT))
			{
				logService.logGebeurtenis(LogGebeurtenis.BRIEFPROJECT_GROEP_TOEGEVOEGD, ingelogdeGebruiker,
					"Briefproject: " + groep.getProject().getNaam() + " Groep: " + groep.getNaam() + " Populatie: " + groep.getPopulatie());
			}
			else
			{
				logService.logGebeurtenis(LogGebeurtenis.PROJECT_GROEP_TOEGEVOEGD, ingelogdeGebruiker,
					"Project: " + groep.getProject().getNaam() + " Groep: " + groep.getNaam() + " Populatie: " + groep.getPopulatie());
			}

		}
		catch (Exception e)
		{
			LOG.error("Er is een fout opgetreden bestand is niet geimporteerd", e);
			putMeldingInHash(meldingen, ProjectImportMelding.ERROR, "Er is een fout opgetreden bestand is niet geimporteerd");
		}
		finally
		{
			try
			{
				if (csvReader != null)
				{
					csvReader.close();
				}
			}
			catch (IOException e)
			{
				LOG.error(e.getMessage(), e);
			}
		}
		if (!skipFouten && error)
		{
			putMeldingInHash(meldingen, ProjectImportMelding.INFO,
				"Er zouden met het vinkje skip fouten " + projectClienten.size() + " clienten worden geselecteerd en " + foutClienten + " overgeslagen.");
		}
		return meldingen;
	}

	@Override
	public Map<ProjectImportMelding, List<String>> inactiveerProjectClienten(ProjectInactiveerDocument projectInactiveerDocument)
	{
		boolean error = false;
		boolean andereClientenGeimporteerd = false;
		Project project = projectInactiveerDocument.getProject();
		UploadDocument upload = projectInactiveerDocument.getUploadDocument();
		SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy");
		Date nu = currentDateSupplier.getDate();
		List<ProjectClient> projectClienten = new ArrayList<>();
		Map<ProjectImportMelding, List<String>> meldingen = new HashMap<>();

		int regelnummer = 0;

		CSVReader csvReader = null;
		try
		{
			csvReader = new CSVReader(new FileReader(fileService.load(upload)), ';');

			for (String[] line : csvReader.readAll())
			{
				regelnummer++;

				if (line.length < 2)
				{
					error = true;
					putMeldingInHash(meldingen, ProjectImportMelding.ERROR,
						"Bsn en geboortedatum is verplicht. Wanneer deze aanwezig zijn controleer op scheidingsteken tussen velden op een ;");
					return meldingen;
				}

				String bsn = line[0];
				if (!StringUtils.isNotBlank(bsn) || !StringUtils.isNumeric(bsn))
				{

					continue;
				}

				if (bsn.length() < 9)
				{
					bsn = StringUtils.leftPad(bsn, 9, '0');
				}

				String datum = line[1];
				Date geboortedatum = null;
				try
				{
					geboortedatum = format.parse(datum);
				}
				catch (ParseException e)
				{
					String melding = "Regel " + regelnummer + ": Verkeerde formaat datum aangeleverder, verwacht: dag-maand-jaar. (Voorbeeld. 12-01-1950)";
					LOG.error(melding);
					putMeldingInHash(meldingen, ProjectImportMelding.ERROR, melding);
					error = true;
					continue;
				}

				Client client = clientService.getClientByBsn(bsn);
				if (client != null && format.format(client.getPersoon().getGeboortedatum()).equals(format.format(geboortedatum)))
				{
					ProjectClient pClient = ProjectUtil.getHuidigeProjectClient(client, currentDateSupplier.getDate(), false);
					ProjectClient pClientInactief = ProjectUtil.getProjectClientVanProject(client, project);
					if (pClient != null && pClient.getProject().equals(project) && pClient.getActief())
					{
						pClient.setActief(false);
						pClient.setProjectInactiefReden(ProjectInactiefReden.INACTIVATIE_DOOR_UPLOAD);
						pClient.setProjectInactiefDatum(nu);
						pClient.setProjectInactiveerDocument(projectInactiveerDocument);
						projectClienten.add(pClient);
						hibernateService.saveOrUpdate(pClient);
						andereClientenGeimporteerd = true;
						putMeldingInHash(meldingen, ProjectImportMelding.SUCCES,
							"Regel " + regelnummer + ": Cliënt met BSN " + bsn + " is succesvol geinactiveerd voor het project.");
					}
					else if (pClient == null && pClientInactief != null && !pClientInactief.getActief())
					{
						putMeldingInHash(meldingen, ProjectImportMelding.ERROR, "Regel " + regelnummer + ": Cliënt met BSN " + bsn + " is al inactief voor het project.");
						error = true;
					}
					else
					{
						putMeldingInHash(meldingen, ProjectImportMelding.ERROR,
							"Regel " + regelnummer + ": Cliënt met BSN " + bsn + " valt niet onder dit project en wordt daarom niet geinactiveerd");
						error = true;
					}
				}
				else
				{
					putMeldingInHash(meldingen, ProjectImportMelding.ERROR, "Regel " + regelnummer + ": Er is geen cliënt gevonden met dit BSN en geboortedatum.");
					error = true;
				}

			}
			if (!error)
			{
				putMeldingInHash(meldingen, ProjectImportMelding.MELDING,
					"Bestand is verwerkt, " + projectClienten.size() + " clienten van het project zijn succesvol geinactiveerd.");
			}
			else
			{
				putMeldingInHash(meldingen, ProjectImportMelding.MELDING, "Bestand is deels of niet verwerkt, Er zijn " + projectClienten.size() + " clienten geinactiveerd.");
			}
			if (andereClientenGeimporteerd)
			{
				String melding = projectClienten.size() + " clienten geinactiveerd voor project " + projectInactiveerDocument.getProject().getNaam() + " door file "
					+ upload.getNaam();
				logService.logGebeurtenis(LogGebeurtenis.PROJECT_INACTIVEREN_UPLOAD, projectInactiveerDocument.getGeuploadDoor(), melding);
			}

		}
		catch (Exception e)
		{
			LOG.error("Er is een fout opgetreden bestand is niet geimporteerd", e);
			putMeldingInHash(meldingen, ProjectImportMelding.ERROR, "Er is een fout opgetreden bestand is niet geimporteerd");
		}
		finally
		{
			if (csvReader != null)
			{
				try
				{
					csvReader.close();
				}
				catch (IOException e)
				{
					LOG.error(e.getMessage(), e);
				}
			}
		}

		return meldingen;
	}

	private Map<ProjectImportMelding, List<String>> putMeldingInHash(Map<ProjectImportMelding, List<String>> meldingen, ProjectImportMelding key, String value)
	{
		if (!meldingen.containsKey(key))
		{
			meldingen.put(key, new ArrayList<>());
		}
		meldingen.get(key).add(value);
		return meldingen;
	}

	private Map<ProjectImportMelding, List<String>> putAllMeldingInHash(Map<ProjectImportMelding, List<String>> meldingen, List<String> nieuweMeldingen)
	{
		for (String melding : nieuweMeldingen)
		{
			putMeldingInHash(meldingen, ProjectImportMelding.ERROR, melding);
		}
		return meldingen;
	}

	private List<String> valideerImportOpFoutenClient(ProjectGroep groep, Client client, Date geboortedatum, int regelNummer)
	{
		List<String> meldingen = new ArrayList<>();

		if (client == null || client.getPersoon() == null || client.getPersoon().getGeboortedatum() == null)
		{
			meldingen.add("Regel " + regelNummer + ": Er is geen cliënt gevonden met dit BSN en geboortedatum.");
			return meldingen;
		}
		Date geboorteDatumClient = client.getPersoon().getGeboortedatum();
		SimpleDateFormat formatter = new SimpleDateFormat("dd-MM-yyyy");
		if (!formatter.format(geboorteDatumClient).equals(formatter.format(geboortedatum)))
		{
			meldingen.add("Regel " + regelNummer + ": Er is geen cliënt gevonden met dit BSN en geboortedatum.");
			return meldingen;
		}
		if (client.getPersoon().getOverlijdensdatum() != null)
		{
			meldingen.add("Regel " + regelNummer + ": Deze cliënt is al overleden. Verwijder deze cliënt uit de xls.");
		}
		if (client.getPersoon().getDatumVertrokkenUitNederland() != null)
		{
			meldingen.add("Regel " + regelNummer + ": Deze cliënt is reeds vertrokken uit Nederland. Verwijder deze cliënt uit de xls.");
		}
		if (!GbaStatus.INDICATIE_AANWEZIG.equals(client.getGbaStatus()))
		{
			meldingen.add("Regel " + regelNummer + ": Deze cliënt heeft een verkeerde gba status. Verwijder deze cliënt uit de xls.");
		}

		Project project = groep.getProject();
		List<Bevolkingsonderzoek> excludeerAfmeldingOnderzoeken = project.getExcludeerAfmelding();
		List<Bevolkingsonderzoek> clientOnderzoeken = clientService.totWelkeBevolkingsonderzoekenHoortDezeClient(client);
		if (excludeerAfmeldingOnderzoeken.size() > 0 && clientOnderzoeken.size() > 0)
		{

			for (Bevolkingsonderzoek excludeerOnderzoek : excludeerAfmeldingOnderzoeken)
			{
				if (clientOnderzoeken.contains(excludeerOnderzoek)
					&& AfmeldingUtil.isAfgemeld(clientService.getDossier(client, excludeerOnderzoek)))
				{
					meldingen.add("Regel " + regelNummer + ": Deze cliënt heeft zich afgemeld bij het onderzoek: " + excludeerOnderzoek.getAfkorting()
						+ ", en deze is geëxcludeerd. Verwijder deze cliënt uit de xls.");
				}
			}
		}

		if (groep.getProject().getType().equals(ProjectType.PROJECT))
		{
			if (ProjectUtil.getHuidigeProjectClient(client, currentDateSupplier.getDate(), false) != null)
			{
				meldingen.add("Regel " + regelNummer + ": Deze cliënt doet al mee aan het project. Verwijder deze cliënt uit de xls.");
			}
			else if (ProjectUtil.getProjectClientVanProject(client, project) != null)
			{
				meldingen.add("Regel " + regelNummer + ": Deze cliënt is al aan dit project gekoppeld of is al gekoppeld geweest. Verwijder deze cliënt uit de xls.");
			}
		}

		if (Boolean.TRUE.equals(project.getExcludeerBezwaar()) && (BezwaarUtil.isBezwaarActiefVoor(client, BezwaarType.GEEN_WETENSCHAPPELIJK_ONDERZOEK, Bevolkingsonderzoek.COLON)
			|| BezwaarUtil.isBezwaarActiefVoor(client, BezwaarType.GEEN_WETENSCHAPPELIJK_ONDERZOEK, Bevolkingsonderzoek.CERVIX)))
		{
			meldingen.add("Regel " + regelNummer + ": Deze cliënt heeft een bezwaar op wetenschappelijk onderzoek. Verwijder deze cliënt uit de xls.");
		}
		if (Boolean.TRUE.equals(project.getExcludeerBezwaar()) && (BezwaarUtil.isBezwaarActiefVoor(client, BezwaarType.GEEN_KWALITEITSWAARBORGING, Bevolkingsonderzoek.COLON)
			|| BezwaarUtil.isBezwaarActiefVoor(client, BezwaarType.GEEN_WETENSCHAPPELIJK_ONDERZOEK, Bevolkingsonderzoek.CERVIX)))
		{
			meldingen.add("Regel " + regelNummer + ": Deze cliënt heeft een bezwaar op kwaliteitsborging. Verwijder deze cliënt uit de xls.");
		}

		if (project.getExcludeerOpenRonde().contains(Bevolkingsonderzoek.COLON)
			&& client.getColonDossier().getLaatsteScreeningRonde() != null
			&& DateUtil.compareBefore(client.getColonDossier().getLaatsteScreeningRonde().getCreatieDatum(), project.getStartDatum()))
		{
			meldingen.add("Regel " + regelNummer
				+ ": Deze cliënt is al gestart met een DK screeningronde terwijl dit project een excludeer op open ronde heeft. Verwijder deze cliënt uit de xls.");
		}
		if (project.getExcludeerOpenRonde().contains(Bevolkingsonderzoek.CERVIX) && client.getCervixDossier() != null
			&& client.getCervixDossier().getLaatsteScreeningRonde() != null
			&& DateUtil.compareBefore(client.getCervixDossier().getLaatsteScreeningRonde().getCreatieDatum(), project.getStartDatum()))
		{
			meldingen.add("Regel " + regelNummer
				+ ": Deze cliënt is al gestart met een BMHK screeningronde terwijl dit project een excludeer op open ronde heeft. Verwijder deze cliënt uit de xls.");
		}
		if (project.getExcludeerOpenRonde().contains(Bevolkingsonderzoek.MAMMA) && client.getMammaDossier() != null
			&& client.getMammaDossier().getLaatsteScreeningRonde() != null
			&& DateUtil.compareBefore(client.getMammaDossier().getLaatsteScreeningRonde().getCreatieDatum(), project.getStartDatum()))
		{
			meldingen.add("Regel " + regelNummer
				+ ": Deze cliënt is al gestart met een BK screeningronde terwijl dit project een excludeer op open ronde heeft. Verwijder deze cliënt uit de xls.");
		}
		return meldingen;
	}

	@Override
	public boolean verwerkGroepClienten(List<Client> clienten, ProjectGroep groep)
	{
		return false;
	}

	@Override
	public long getCountProjectBriefActies(ProjectBriefActie actie)
	{
		return projectDao.getCountProjectBriefActies(actie);
	}

	@Override
	public Iterator<ProjectBriefActie> getProjectBriefActies(ProjectBriefActie actie, long first, long count, SortState<String> sortState)
	{
		return projectDao.getProjectBriefActies(actie, first, count, sortState);
	}

	@Override
	public List<ProjectClient> getValideClientenVanProject(Project project, ProjectBriefActie definitie)
	{
		return projectDao.getValideClientenVanProject(project, definitie);
	}

	@Override
	public List<Project> getAllProjectenWhereProjectBriefActieHasBriefType(BriefType type)
	{
		return projectDao.getAllProjectenWhereProjectBriefActieHasBriefType(type);
	}

	@Override
	public List<ProjectBrief> getAllProjectBriefForHerinnering(ProjectBriefActie actie, Date verstuurdOp)
	{
		return projectDao.getAllProjectBriefForHerinnering(actie, verstuurdOp);
	}

	@Override
	public List<ProjectType> getProjectTypes(InstellingGebruiker instellingGebruiker, Actie minimumActie, boolean checkBvo)
	{
		List<ProjectType> projectTypes = new ArrayList<>();

		for (ProjectType projectType : ProjectType.values())
		{

			ToegangLevel level = autorisatieService.getToegangLevel(instellingGebruiker, minimumActie, checkBvo, projectType.getRecht());
			if (level != null)
			{
				projectTypes.add(projectType);
			}
		}
		return projectTypes;
	}

	@Override
	public String generateVragenlijstUrl(ProjectBrief projectBrief)
	{
		StringBuilder sb = new StringBuilder(applicationUrl);
		if (!applicationUrl.endsWith("/"))
		{
			sb.append("/");
		}
		sb.append(VRAGENLIJST_PREFIX);
		sb.append(generateVragenlijstKey(projectBrief));
		return sb.toString();
	}

	@Override
	public String generateVragenlijstKey(ProjectBrief projectBrief)
	{
		StringBuilder sb = new StringBuilder("B");
		sb.append(projectBrief.getId().toString());
		String controleGetal = projectBrief.getProjectClient().getClient().getId().toString();
		sb.append(controleGetal.substring(controleGetal.length() - 4));
		for (int i = sb.length() - 4; i > 0; i = i - 4)
		{
			sb.insert(i, '-');
		}
		return sb.toString();
	}

	@Override
	public ProjectBrief getProjectBriefFromVragenlijstKey(String vragenlijstKey)
	{
		if (vragenlijstKey != null && vragenlijstKey.length() > 4)
		{
			String key = vragenlijstKey.replaceAll("-", "");
			String controleGetal = key.substring(key.length() - 4);
			Long projectBriefId = Long.valueOf(key.substring(1, key.length() - 4));
			if (key.startsWith("B"))
			{
				ProjectBrief projectBrief = hibernateService.get(ProjectBrief.class, projectBriefId);
				if (projectBrief != null)
				{
					String projectClientId = projectBrief.getProjectClient().getClient().getId().toString();
					if (projectClientId.substring(projectClientId.length() - 4).equals(controleGetal.substring(controleGetal.length() - 4)))
					{
						return projectBrief;
					}
				}
			}
		}
		return null;
	}

	@Override
	public boolean isVragenlijstGekoppeldAanNietBeeindigdProject(Long vragenlijstId)
	{
		return projectDao.isVragenlijstGekoppeldAanNietBeeindigdProject(vragenlijstId);
	}

	@Override
	public boolean isVragenlijstGekoppeldAanProject(Long vragenlijstId)
	{
		return projectDao.isVragenlijstGekoppeldAanProject(vragenlijstId);
	}

	@Override
	public Long getAantalInactieveProjectClientenVanProject(Project project)
	{
		return projectDao.getAantalInactieveProjectClientenVanProject(project);
	}

	@Override
	public Long getAantalInactieveProjectClientenVanProjectGroep(ProjectGroep groep)
	{
		return projectDao.getAantalInactieveProjectClientenVanProjectGroep(groep);
	}

	@Override
	public Long getAantalProjectClientenVanProject(Project project)
	{
		return projectDao.getAantalInactieveProjectClientenVanProject(project);
	}

	@Override
	public Long getAantalProjectClientenVanProjectGroep(ProjectGroep groep)
	{
		return projectDao.getAantalProjectClientenVanProjectGroep(groep);
	}

	@Override
	public ProjectBriefActie getProjectBriefActie(Client client, BriefType briefType)
	{
		return projectDao.getProjectBriefActie(client, briefType);
	}

	@Override
	public boolean addVragenlijstAanTemplate(MailMergeContext context, Document chunkDocument, ProjectBriefActie actie, ProjectBrief projectBrief) throws Exception
	{
		ProjectVragenlijst vragenlijst = actie.getVragenlijst();
		if (vragenlijst != null && ProjectVragenlijstUitzettenVia.isPapier(actie.getProjectVragenlijstUitzettenVia()))
		{
			Document vragenlijstDocument = null;
			ScreenitFormulierInstantie vragenlijstFormulierInstantie = null;
			vragenlijstFormulierInstantie = vragenlijst.getFormulierInstantie();

			if (vragenlijstFormulierInstantie != null)
			{
				if (vragenlijstFormulierInstantie.getTemplateVanGebruiker() == null)
				{
					vragenlijstDocument = asposeService.processVragenlijst(context, vragenlijstFormulierInstantie, true);
				}
				else
				{
					File vragenlijstTemplate = fileService.load(vragenlijstFormulierInstantie.getTemplateVanGebruiker());
					byte[] vragenlijstTemplateBytes = FileUtils.readFileToByteArray(vragenlijstTemplate);
					vragenlijstDocument = asposeService.processDocument(vragenlijstTemplateBytes, context);
				}
			}
			if (vragenlijstDocument != null)
			{
				chunkDocument.getLastSection().getHeadersFooters().linkToPrevious(false);
				chunkDocument.appendDocument(vragenlijstDocument, ImportFormatMode.KEEP_SOURCE_FORMATTING);

				ProjectVragenlijstAntwoordenHolder holder = new ProjectVragenlijstAntwoordenHolder();
				holder.setStatus(ProjectVragenlijstStatus.AANGEMAAKT);
				holder.setVragenlijst(vragenlijst);

				VragenlijstAntwoorden<ProjectVragenlijstAntwoordenHolder> antwoorden = new VragenlijstAntwoorden<ProjectVragenlijstAntwoordenHolder>();
				antwoorden.setFormulierInstantie(vragenlijstFormulierInstantie);
				antwoorden.setAntwoordenHolder(holder);
				holder.setVragenlijstAntwoorden(antwoorden);
				FormulierResultaatImpl resultaat = new FormulierResultaatImpl();
				resultaat.setFormulierInstantie(vragenlijstFormulierInstantie);
				antwoorden.setResultaat(resultaat);
				hibernateService.saveOrUpdate(antwoorden);
				hibernateService.saveOrUpdate(holder);
				hibernateService.saveOrUpdate(resultaat);

				if (projectBrief != null)
				{
					projectBrief.setVragenlijstAntwoordenHolder(holder);
				}
			}
			return true;
		}
		return false;
	}

	@Override
	public void verwerkProjectBestand(ProjectBestand projectBestand)
	{
		executerService.submit(new ProjectBestandVerwerkThread(projectBestand.getId()));
	}

	@Override
	public void verwerkProjectUitslagen(ProjectBestand uitslagenBestand)
	{
		executerService.submit(new ProjectUitslagVerwerkThread(uitslagenBestand.getId()));
	}

	@Override
	public List<ProjectGroep> getActieveProjectGroepenVoorUitnodigingDK()
	{
		return projectDao.getActieveProjectGroepenVoorUitnodigingDK();
	}

	@Override
	public void projectAttribuutOpslaan(ProjectAttribuut attribuut)
	{
		String naamAttribuut = attribuut.getNaam();
		String projectNaam = attribuut.getProject().getNaam();

		naamAttribuut = naamAttribuut.replaceAll(" ", "").toLowerCase();
		projectNaam = projectNaam.replaceAll(" ", "").toLowerCase();

		attribuut.setMergeField("_" + projectNaam + "_" + naamAttribuut);
		hibernateService.saveOrUpdate(attribuut);
	}

	@Override
	public void queueProjectBestandVoorAttributen(Project project, ProjectGroep groep, ProjectBestand bestand, String contentType, String filenaam, File file,
		Account loggedInAccount) throws IOException
	{

		Date nu = currentDateSupplier.getDate();
		UploadDocument upload = new UploadDocument();
		upload.setContentType(contentType);
		upload.setNaam(filenaam);
		upload.setFile(file);
		upload.setActief(true);
		fileService.saveOrUpdateUploadDocument(upload, FileStoreLocation.PROJECT_BESTAND, project.getId());

		bestand.setType(ProjectBestandType.ATTRIBUTEN);
		bestand.setUploadDocument(upload);
		bestand.setUploadDatum(nu);
		bestand.setStatus(BestandStatus.NOG_TE_VERWERKEN);
		bestand.setStatusDatum(nu);
		bestand.setGroep(groep);
		bestand.setProject(project);

		hibernateService.saveOrUpdate(bestand);

		verwerkProjectBestand(bestand);

		logService.logGebeurtenis(LogGebeurtenis.PROJECT_BESTAND_TOEGEVOEGD, loggedInAccount, getLoggingMelding(bestand));
	}

	@Override
	public void queueProjectBestandVoorUitslagen(Project project, ProjectBestand uitslagenBestand, String contentType, String filenaam, File file, Account loggedInAccount)
		throws IOException
	{
		Date nu = currentDateSupplier.getDate();
		UploadDocument upload = new UploadDocument();
		upload.setContentType(contentType);
		upload.setNaam(filenaam);
		upload.setFile(file);
		upload.setActief(true);
		fileService.saveOrUpdateUploadDocument(upload, FileStoreLocation.PROJECT_BESTAND, project.getId());

		uitslagenBestand.setType(ProjectBestandType.UITSLAGEN);
		uitslagenBestand.setUploadDocument(upload);
		uitslagenBestand.setUploadDatum(nu);
		uitslagenBestand.setStatus(BestandStatus.NOG_TE_VERWERKEN);
		uitslagenBestand.setStatusDatum(nu);
		uitslagenBestand.setProject(project);

		hibernateService.saveOrUpdate(uitslagenBestand);

		verwerkProjectUitslagen(uitslagenBestand);

		logService.logGebeurtenis(LogGebeurtenis.PROJECT_UITSLAG_TOEGEVOEGD, loggedInAccount, getLoggingMelding(uitslagenBestand));
	}

	@Override
	public void queueProjectBestandVoorClientWijzigingen(Project project, ProjectBestand projectBestand, UploadDocument uploadDocument, String contentType, String filenaam,
		File file, Account loggedInAccount) throws IOException
	{
		Date nu = currentDateSupplier.getDate();
		fileService.saveOrUpdateUploadDocument(uploadDocument, FileStoreLocation.PROJECT_INACTIVEREN, project.getId());

		projectBestand.setUploadDocument(uploadDocument);
		projectBestand.setUploadDatum(nu);
		projectBestand.setStatus(BestandStatus.NOG_TE_VERWERKEN);
		projectBestand.setStatusDatum(nu);
		projectBestand.setProject(project);

		hibernateService.saveOrUpdate(projectBestand);

		verwerkProjectBestand(projectBestand);

		logService.logGebeurtenis(LogGebeurtenis.PROJECT_BESTAND_TOEGEVOEGD, loggedInAccount, getLoggingMelding(projectBestand));
	}

	@Override
	public void queueProjectBestandVoorPopulatie(ProjectGroep groep, String contentType, String filenaam, File file, Account loggedInAccount) throws IOException
	{
		Date nu = currentDateSupplier.getDate();
		UploadDocument upload = new UploadDocument();
		upload.setContentType(contentType);
		upload.setNaam(filenaam);
		upload.setFile(file);
		upload.setActief(true);
		fileService.saveOrUpdateUploadDocument(upload, FileStoreLocation.PROJECT_BESTAND, groep.getProject().getId());

		ProjectBestand bestand = new ProjectBestand();
		bestand.setType(ProjectBestandType.POPULATIE);
		bestand.setPopulatie(true);
		bestand.setUploadDocument(upload);
		bestand.setUploadDatum(nu);
		bestand.setStatus(BestandStatus.NOG_TE_VERWERKEN);
		bestand.setStatusDatum(nu);
		bestand.setGroep(groep);
		bestand.setProject(groep.getProject());

		groep.setPopulatie(0);
		groep.setClienten(new ArrayList<>());
		hibernateService.saveOrUpdateAll(groep, bestand);

		verwerkProjectBestand(bestand);

		logService.logGebeurtenis(LogGebeurtenis.PROJECT_BESTAND_TOEGEVOEGD, loggedInAccount, getLoggingMelding(bestand));
	}

	private String getLoggingMelding(ProjectBestand bestand)
	{
		String melding;
		if (bestand.getType().equals(ProjectBestandType.UITSLAGEN))
		{
			melding = "Uitslagen geupload met naam " + bestand.getUploadDocument().getNaam() + " wordt toegepast op hele project";
		}
		else
		{
			melding = "Bestand geupload met naam " + bestand.getUploadDocument().getNaam() + " wordt ";
			if (bestand.isAttributen() && bestand.isPopulatie())
			{
				melding += "verwerkt voor populatie en attributen ";
			}
			else if (bestand.isPopulatie())
			{
				melding += "verwerkt voor populatie ";
			}
			else if (bestand.isAttributen())
			{
				melding += "verwerkt voor attributen ";
			}
			if (StringUtils.isNotEmpty(bestand.getToepassenOp()))
			{
				melding += "toegepast op " + bestand.getToepassenOp();
			}
			else
			{
				melding += "toegepast op hele project";
			}
		}
		melding += " (" + bestand.getProject().getNaam() + ")";
		return melding;
	}

	@Override
	public void updateWachtOpStartProject()
	{
		projectDao.resetWachtOpStartProject();
		projectDao.setNieuwWachtOpStartProject(currentDateSupplier.getDate());
	}
}
