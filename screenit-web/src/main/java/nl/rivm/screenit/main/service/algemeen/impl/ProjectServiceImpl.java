package nl.rivm.screenit.main.service.algemeen.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import nl.rivm.screenit.main.service.algemeen.ProjectService;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectAttribuut;
import nl.rivm.screenit.model.project.ProjectBestand;
import nl.rivm.screenit.model.project.ProjectBestandType;
import nl.rivm.screenit.model.project.ProjectBestandVerwerkingEntry;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectClientAttribuut;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.rivm.screenit.model.project.ProjectType;
import nl.rivm.screenit.model.project.Project_;
import nl.rivm.screenit.repository.algemeen.ProjectAttribuutRepository;
import nl.rivm.screenit.repository.algemeen.ProjectBriefActieRepository;
import nl.rivm.screenit.repository.algemeen.ProjectClientRepository;
import nl.rivm.screenit.repository.algemeen.ProjectRepository;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.impl.ProjectBestandVerwerkThread;
import nl.rivm.screenit.service.impl.ProjectUitslagVerwerkThread;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.HibernateObjectSpecification.filterNietId;
import static nl.rivm.screenit.specification.algemeen.ProjectAttribuutSpecification.heeftMergeField;
import static nl.rivm.screenit.specification.algemeen.ProjectAttribuutSpecification.heeftNaam;
import static nl.rivm.screenit.specification.algemeen.ProjectAttribuutSpecification.heeftProject;
import static nl.rivm.screenit.specification.algemeen.ProjectSpecification.heeftBriefType;
import static nl.rivm.screenit.specification.algemeen.ProjectSpecification.heeftEindDatumNaVandaag;
import static nl.rivm.screenit.specification.algemeen.ProjectSpecification.heeftType;

@Service
public class ProjectServiceImpl implements ProjectService
{
	private final ExecutorService executerService;

	@Autowired
	private AutorisatieService autorisatieService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired
	private ProjectRepository projectRepository;

	@Autowired
	private ProjectClientRepository projectClientRepository;

	@Autowired
	private ProjectGroepDataProviderServiceImpl projectGroepDataProviderService;

	@Autowired
	private ProjectClientDataProviderServiceImpl projectClientDataProviderService;

	@Autowired
	private ProjectBriefActiesDataProviderServiceImpl projectBriefActiesDataProviderService;

	@Autowired
	private ProjectAttributenDataProviderServiceImpl projectAttributenDataProviderService;

	@Autowired
	private ProjectBestandDataProviderServiceImpl projectBestandDataProviderService;

	@Autowired
	private ProjectBestandVerwerkingDataProviderServiceImpl projectBestandVerwerkingDataProviderService;

	@Autowired
	private ClientProjectAttributenDataProviderServiceImpl clientProjectAttributenDataProviderService;

	@Autowired
	private ProjectBriefActieRepository projectBriefActieRepository;

	@Autowired
	private ProjectAttribuutRepository projectAttribuutRepository;

	public ProjectServiceImpl()
	{
		executerService = Executors.newSingleThreadExecutor();
	}

	@Override
	@Transactional
	public void saveOrUpdateProject(Project project, InstellingGebruiker instellingGebruiker)
	{
		if (project.getId() == null)
		{
			if (project.getType().equals(ProjectType.BRIEFPROJECT))
			{
				logService.logGebeurtenis(LogGebeurtenis.BRIEFPROJECT_AANGEMAAKT, instellingGebruiker, "Briefprojectnaam: " + project.getNaam());
			}
			else
			{
				logService.logGebeurtenis(LogGebeurtenis.PROJECT_AANGEMAAKT, instellingGebruiker, "Projectnaam: " + project.getNaam());
			}
		}
		else
		{
			if (project.getType().equals(ProjectType.BRIEFPROJECT))
			{
				logService.logGebeurtenis(LogGebeurtenis.BRIEFPROJECT_GEWIJZIGD, instellingGebruiker, "Briefprojectnaam: " + project.getNaam());
			}
			else
			{
				logService.logGebeurtenis(LogGebeurtenis.PROJECT_GEWIJZIGD, instellingGebruiker, "Projectnaam: " + project.getNaam());
			}
		}

		hibernateService.saveOrUpdate(project);
	}

	@Override
	public List<Project> getProjectenVanType(ProjectType projectType)
	{
		return projectRepository.findAll(heeftType(projectType), Sort.by(Sort.Order.asc(Project_.NAAM)));
	}

	@Override
	public List<Project> getProjecten()
	{
		return projectRepository.findAll(Sort.by(Sort.Order.asc(Project_.NAAM)));
	}

	@Override
	public List<ProjectGroep> getGroepen(ProjectGroep zoekObject, long first, long count, Sort sort)
	{
		return projectGroepDataProviderService.findPage(first, count, zoekObject, sort);
	}

	@Override
	public long getCountGroepen(ProjectGroep zoekObject)
	{
		return projectGroepDataProviderService.size(zoekObject);
	}

	@Override
	public List<ProjectClient> getClientProjecten(ProjectClient client, long first, long count, Sort sort)
	{
		return projectClientDataProviderService.findPage(first, count, client, sort);
	}

	@Override
	public long getCountClientProjecten(ProjectClient client)
	{
		return projectClientDataProviderService.size(client);
	}

	@Override
	public List<ProjectBriefActie> getProjectBriefActies(ProjectBriefActie zoekObject, long first, long count, Sort sort)
	{
		return projectBriefActiesDataProviderService.findPage(first, count, zoekObject, sort);
	}

	@Override
	public long getCountProjectBriefActies(ProjectBriefActie zoekObject)
	{
		return projectBriefActiesDataProviderService.size(zoekObject);
	}

	@Override
	public List<Project> getAllProjectenWhereProjectBriefActieHasBriefType(BriefType type)
	{
		var vandaag = currentDateSupplier.getLocalDate();

		return projectRepository.findWith(heeftBriefType(type)
			.and(heeftEindDatumNaVandaag(vandaag)), q -> q.distinct().all());
	}

	@Override
	public List<ProjectType> getProjectTypes(InstellingGebruiker instellingGebruiker, Actie minimumActie, boolean checkBvo)
	{
		List<ProjectType> projectTypes = new ArrayList<>();

		for (var projectType : ProjectType.values())
		{

			var level = autorisatieService.getToegangLevel(instellingGebruiker, minimumActie, checkBvo, projectType.getRecht());
			if (level != null)
			{
				projectTypes.add(projectType);
			}
		}
		return projectTypes;
	}

	@Override
	public Long getAantalInactieveProjectClientenVanProject(Project project)
	{
		return projectClientRepository.countByProjectAndActief(project, false);
	}

	@Override
	public Long getAantalInactieveProjectClientenVanProjectGroep(ProjectGroep groep)
	{
		return projectClientRepository.countByGroepAndActief(groep, false);
	}

	@Override
	public Long getAantalProjectClientenVanProject(Project project)
	{
		return projectClientRepository.countByProject(project);
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
	public void projectAttribuutOpslaan(ProjectAttribuut attribuut)
	{
		var naamAttribuut = attribuut.getNaam();
		var projectNaam = attribuut.getProject().getNaam();

		naamAttribuut = naamAttribuut.replaceAll(" ", "").toLowerCase();
		projectNaam = projectNaam.replaceAll(" ", "").toLowerCase();

		attribuut.setMergeField("_" + projectNaam + "_" + naamAttribuut);
		hibernateService.saveOrUpdate(attribuut);
	}

	@Override
	public void queueProjectBestandVoorAttributen(Project project, ProjectGroep groep, ProjectBestand bestand, String contentType, String filenaam, File file,
		Account loggedInAccount) throws IOException
	{
		var nu = currentDateSupplier.getDate();
		var upload = new UploadDocument();
		upload.setContentType(contentType);
		upload.setNaam(filenaam);
		upload.setFile(file);
		upload.setActief(true);
		uploadDocumentService.saveOrUpdate(upload, FileStoreLocation.PROJECT_BESTAND, project.getId());

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
		var nu = currentDateSupplier.getDate();
		var upload = new UploadDocument();
		upload.setContentType(contentType);
		upload.setNaam(filenaam);
		upload.setFile(file);
		upload.setActief(true);
		uploadDocumentService.saveOrUpdate(upload, FileStoreLocation.PROJECT_BESTAND, project.getId());

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
		var nu = currentDateSupplier.getDate();
		uploadDocumentService.saveOrUpdate(uploadDocument, FileStoreLocation.PROJECT_INACTIVEREN, project.getId());

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
		var nu = currentDateSupplier.getDate();
		var upload = new UploadDocument();
		upload.setContentType(contentType);
		upload.setNaam(filenaam);
		upload.setFile(file);
		upload.setActief(true);
		uploadDocumentService.saveOrUpdate(upload, FileStoreLocation.PROJECT_BESTAND, groep.getProject().getId());

		var bestand = new ProjectBestand();
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
	@Transactional
	public void verwijderProjectGroep(ProjectGroep groep, Account loggedInAccount)
	{
		var project = groep.getProject();
		project.getGroepen().remove(groep);
		List<UploadDocument> documentenTeVerwijderen = new ArrayList<>();
		if (groep.getProjectImport() != null && groep.getProjectImport().getUploadDocument() != null)
		{
			documentenTeVerwijderen.add(groep.getProjectImport().getUploadDocument());
		}
		groep.getProjectBestanden().stream()
			.filter(b -> b.getUploadDocument() != null)
			.forEach(b -> documentenTeVerwijderen.add(b.getUploadDocument()));
		groep.getClienten().stream()
			.filter(c -> c.getProjectInactiveerDocument() != null && c.getProjectInactiveerDocument().getUploadDocument() != null)
			.forEach(c -> documentenTeVerwijderen.add(c.getProjectInactiveerDocument().getUploadDocument()));
		hibernateService.delete(groep);
		hibernateService.saveOrUpdate(project);

		documentenTeVerwijderen.forEach(d -> uploadDocumentService.delete(d));

		if (project.getType().equals(ProjectType.BRIEFPROJECT))
		{
			logService.logGebeurtenis(LogGebeurtenis.BRIEFPROJECT_GROEP_VERWIJDERD, loggedInAccount,
				"Briefproject: " + project.getNaam() + " Groep: " + groep.getNaam() + " Populatie: " + groep.getPopulatie());
		}
		else
		{
			logService.logGebeurtenis(LogGebeurtenis.PROJECT_GROEP_VERWIJDERD, loggedInAccount,
				"Project: " + project.getNaam() + " Groep: " + groep.getNaam() + " Populatie: " + groep.getPopulatie());
		}
	}

	@Override
	@Transactional
	public String updateProjectGroepActiefStatus(ProjectGroep groep, Account loggedInAccount)
	{
		groep.setActiefDatum(currentDateSupplier.getDate());
		hibernateService.saveOrUpdate(groep);

		var project = groep.getProject();
		var melding = project.getNaam() + " Groep: " + groep.getNaam() + " Populatie: " + groep.getPopulatie()
			+ " en is op " + (groep.getActief() ? "actief" : "inactief") + " gezet.";
		if (project.getType().equals(ProjectType.BRIEFPROJECT))
		{
			melding = "Briefproject: " + melding;
			logService.logGebeurtenis(LogGebeurtenis.BRIEFPROJECT_GROEP_GEWIJZIGD, loggedInAccount, melding);
		}
		else
		{
			melding = "Project: " + melding;
			logService.logGebeurtenis(LogGebeurtenis.PROJECT_GROEP_GEWIJZIGD, loggedInAccount, melding);
		}
		return melding;
	}

	@Override
	public List<ProjectClientAttribuut> getAttributenVoorProjectClient(ProjectClient zoekObject, long first, long count, Sort sort)
	{
		return clientProjectAttributenDataProviderService.findPage(first, count, zoekObject, sort);
	}

	@Override
	public long getAantalAttributenVoorProjectClient(ProjectClient zoekObject)
	{
		return clientProjectAttributenDataProviderService.size(zoekObject);
	}

	@Override
	public List<ProjectBestandVerwerkingEntry> getProjectBestandVerwerkingEntries(ProjectBestandVerwerkingEntry zoekObject, long first, long count, Sort sort)
	{
		return projectBestandVerwerkingDataProviderService.findPage(first, count, zoekObject, sort);
	}

	@Override
	public long getAantalProjectbestandVerwerkingEntries(ProjectBestandVerwerkingEntry zoekObject)
	{
		return projectBestandVerwerkingDataProviderService.size(zoekObject);
	}

	@Override
	public List<ProjectAttribuut> getProjectAttributen(ProjectAttribuut zoekObject, long first, long count, Sort sort)
	{
		return projectAttributenDataProviderService.findPage(first, count, zoekObject, sort);
	}

	@Override
	public long getAantalProjectAttributen(ProjectAttribuut zoekObject)
	{
		return projectAttributenDataProviderService.size(zoekObject);
	}

	@Override
	public List<ProjectBestand> getProjectBestanden(ProjectBestand zoekObject, long first, long count, Sort sort)
	{
		return projectBestandDataProviderService.findPage(first, count, zoekObject, sort);
	}

	@Override
	public long getAantalProjectBestanden(ProjectBestand zoekObject)
	{
		return projectBestandDataProviderService.size(zoekObject);
	}

	@Override
	public ProjectAttribuut getProjectAttribuut(ProjectAttribuut zoekObject)
	{
		return projectAttribuutRepository.findOne(
			heeftProject(zoekObject.getProject())
				.and(filterNietId(zoekObject.getId()))
				.and(heeftMergeField(zoekObject.getMergeField())
					.or(heeftNaam(zoekObject.getNaam())))).orElse(null);
	}
}
