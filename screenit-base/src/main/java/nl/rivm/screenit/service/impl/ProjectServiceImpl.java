package nl.rivm.screenit.service.impl;

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

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import nl.rivm.screenit.dao.ProjectDao;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Dossier;
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
import nl.rivm.screenit.model.formulieren.ScreenitFormulierInstantie;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectAttribuut;
import nl.rivm.screenit.model.project.ProjectBestand;
import nl.rivm.screenit.model.project.ProjectBestandType;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.rivm.screenit.model.project.ProjectImportMelding;
import nl.rivm.screenit.model.project.ProjectType;
import nl.rivm.screenit.model.project.ProjectVragenlijstAntwoordenHolder;
import nl.rivm.screenit.model.project.ProjectVragenlijstStatus;
import nl.rivm.screenit.model.project.ProjectVragenlijstUitzettenVia;
import nl.rivm.screenit.model.project.Project_;
import nl.rivm.screenit.model.vragenlijsten.VragenlijstAntwoorden;
import nl.rivm.screenit.repository.algemeen.ProjectBriefActieRepository;
import nl.rivm.screenit.repository.algemeen.ProjectBriefRepository;
import nl.rivm.screenit.repository.algemeen.ProjectClientRepository;
import nl.rivm.screenit.repository.algemeen.ProjectRepository;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.ClientDoelgroepService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.ProjectService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.specification.algemeen.ProjectBriefActieSpecification;
import nl.rivm.screenit.util.AfmeldingUtil;
import nl.rivm.screenit.util.BezwaarUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.formulieren2.persistence.resultaat.FormulierResultaatImpl;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.aspose.words.Document;
import com.aspose.words.ImportFormatMode;

import static nl.rivm.screenit.specification.algemeen.ProjectBriefActieSpecification.heeftActieveClientInProjectVoorProjectBriefActie;
import static nl.rivm.screenit.specification.algemeen.ProjectBriefActieSpecification.heeftClient;
import static nl.rivm.screenit.specification.algemeen.ProjectBriefActieSpecification.isProjectBriefActieTypeVervangendeBrief;
import static nl.rivm.screenit.specification.algemeen.ProjectBriefSpecification.heeftActieveClientInProjectVoorProjectBrief;
import static nl.rivm.screenit.specification.algemeen.ProjectBriefSpecification.heeftDefinitieGelijkAanBaseActie;
import static nl.rivm.screenit.specification.algemeen.ProjectBriefSpecification.heeftDefinitieVragenlijst;
import static nl.rivm.screenit.specification.algemeen.ProjectBriefSpecification.heeftGeenVerstuurdeBrief;
import static nl.rivm.screenit.specification.algemeen.ProjectBriefSpecification.heeftPrintDatumNaOfOpDatum;
import static nl.rivm.screenit.specification.algemeen.ProjectBriefSpecification.heeftVragenlijstAntwoordenStatusNullOfNietAfgerond;
import static nl.rivm.screenit.specification.algemeen.ProjectClientSpecification.heeftActieveClient;
import static nl.rivm.screenit.specification.algemeen.ProjectClientSpecification.heeftActieveClientInProjectVoorProjectClient;
import static nl.rivm.screenit.specification.algemeen.ProjectClientSpecification.heeftExcludeerAfmelding;
import static nl.rivm.screenit.specification.algemeen.ProjectClientSpecification.heeftProject;
import static nl.rivm.screenit.specification.algemeen.ProjectClientSpecification.isNietInProjectBrief;
import static nl.rivm.screenit.specification.algemeen.ProjectSpecification.heeftBriefTypeInProject;
import static nl.rivm.screenit.specification.algemeen.ProjectSpecification.heeftEindDatumNaVandaag;
import static nl.rivm.screenit.specification.algemeen.ProjectSpecification.heeftProjectType;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class ProjectServiceImpl implements ProjectService
{
	private static final String VRAGENLIJST_PREFIX = "vragenlijst/";

	private final ExecutorService executerService;

	@Autowired
	private AutorisatieService autorisatieService;

	@Autowired
	private ProjectDao projectDao;

	@Autowired
	private ClientService clientService;

	@Autowired
	private ClientDoelgroepService doelgroepService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired
	private AsposeService asposeService;

	@Autowired
	private String applicationUrl;

	@Autowired
	private ProjectRepository projectRepository;

	@Autowired
	private ProjectClientRepository projectClientRepository;

	@Autowired
	private ProjectBriefRepository projectBriefRepository;

	@Autowired
	private ProjectBriefActieRepository projectBriefActieRepository;

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
				var melding = "Briefprojectnaam: " + project.getNaam();
				logService.logGebeurtenis(LogGebeurtenis.BRIEFPROJECT_AANGEMAAKT, instellingGebruiker, melding);
			}
			else
			{
				var melding = "Projectnaam: " + project.getNaam();
				logService.logGebeurtenis(LogGebeurtenis.PROJECT_AANGEMAAKT, instellingGebruiker, melding);
			}
		}
		else
		{
			if (project.getType().equals(ProjectType.BRIEFPROJECT))
			{
				var melding = "Briefprojectnaam: " + project.getNaam();
				logService.logGebeurtenis(LogGebeurtenis.BRIEFPROJECT_GEWIJZIGD, instellingGebruiker, melding);
			}
			else
			{
				var melding = "Projectnaam: " + project.getNaam();
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
	public List<Project> getProjectenVanType(ProjectType projectType)
	{
		return projectRepository.findAll(heeftProjectType(projectType), Sort.by(Sort.Order.asc(Project_.NAAM)));
	}

	@Override
	public List<Project> getProjecten()
	{
		return projectRepository.findAll(Sort.by(Sort.Order.asc(Project_.NAAM)));
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

	private Map<ProjectImportMelding, List<String>> putAllMeldingInHash(Map<ProjectImportMelding, List<String>> meldingen, List<String> nieuweMeldingen)
	{
		for (var melding : nieuweMeldingen)
		{
			meldingen.computeIfAbsent(ProjectImportMelding.ERROR, s -> new ArrayList<>()).add(melding);
		}
		return meldingen;
	}

	private void valideerRulesPopulatie(ProjectGroep groep, Client client)
	{
		if (client.getPersoon().getOverlijdensdatum() != null)
		{
			throw new IllegalStateException("Deze cliënt is al overleden.");
		}
		if (client.getPersoon().getDatumVertrokkenUitNederland() != null)
		{
			throw new IllegalStateException("Deze cliënt is reeds vertrokken uit Nederland.");
		}
		if (!GbaStatus.INDICATIE_AANWEZIG.equals(client.getGbaStatus()))
		{
			throw new IllegalStateException("Deze cliënt heeft een verkeerde gba status.");
		}

		var project = groep.getProject();
		var excludeerAfmeldingOnderzoeken = project.getExcludeerAfmelding();
		if (excludeerAfmeldingOnderzoeken.size() > 0)
		{

			for (var excludeerOnderzoek : excludeerAfmeldingOnderzoeken)
			{
				if (doelgroepService.behoortTotDoelgroep(client, excludeerOnderzoek)
					&& AfmeldingUtil.isEenmaligOfDefinitefAfgemeld(clientService.getDossier(client, excludeerOnderzoek)))
				{
					throw new IllegalStateException(
						"Deze cliënt heeft zich afgemeld bij het onderzoek: " + excludeerOnderzoek.getAfkorting() + ", en deze is geëxcludeerd.");
				}
			}
		}

		if (groep.getProject().getType().equals(ProjectType.PROJECT))
		{
			var huidigeProjectClient = ProjectUtil.getHuidigeProjectClient(client, currentDateSupplier.getDate(), false);
			if (huidigeProjectClient != null)
			{
				throw new IllegalStateException("Deze cliënt doet al mee aan het project '" + huidigeProjectClient.getProject().getNaam() + "'");
			}
			else if (projectDao.getProjectClient(client, project) != null)
			{
				throw new IllegalStateException("Deze cliënt is al aan dit project gekoppeld of is al gekoppeld geweest.");
			}
		}

		if (Boolean.TRUE.equals(project.getExcludeerBezwaar()) && (BezwaarUtil.isBezwaarActiefVoorEenVanDeOnderzoeken(client, BezwaarType.GEEN_WETENSCHAPPELIJK_ONDERZOEK)))
		{
			throw new IllegalStateException("Deze cliënt heeft een bezwaar op wetenschappelijk onderzoek.");
		}
		if (Boolean.TRUE.equals(project.getExcludeerBezwaar()) && (BezwaarUtil.isBezwaarActiefVoorEenVanDeOnderzoeken(client, BezwaarType.GEEN_KWALITEITSWAARBORGING)))
		{
			throw new IllegalStateException("Deze cliënt heeft een bezwaar op kwaliteitsborging.");
		}

		if (project.getExcludeerOpenRonde().contains(Bevolkingsonderzoek.COLON) && isLaatsteRondeGestartNietNaStartProject(client.getColonDossier(), project))
		{
			throw new IllegalStateException(
				"Deze cliënt is al gestart met een DK screeningronde terwijl dit project een excludeer op open ronde heeft.");
		}
		if (project.getExcludeerOpenRonde().contains(Bevolkingsonderzoek.CERVIX) && isLaatsteRondeGestartNietNaStartProject(client.getCervixDossier(), project))
		{
			throw new IllegalStateException(
				"Deze cliënt is al gestart met een BMHK screeningronde terwijl dit project een excludeer op open ronde heeft.");
		}
		if (project.getExcludeerOpenRonde().contains(Bevolkingsonderzoek.MAMMA) && isLaatsteRondeGestartNietNaStartProject(client.getMammaDossier(), project))
		{
			throw new IllegalStateException(
				"Deze cliënt is al gestart met een BK screeningronde terwijl dit project een excludeer op open ronde heeft.");
		}
	}

	private boolean isLaatsteRondeGestartNietNaStartProject(Dossier dossier, Project project)
	{
		return dossier != null && dossier.getLaatsteScreeningRonde() != null && dossier.getLaatsteScreeningRonde().getCreatieDatum() != null
			&& !DateUtil.toLocalDate(dossier.getLaatsteScreeningRonde().getCreatieDatum()).isAfter(DateUtil.toLocalDate(project.getStartDatum()));
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
		var vandaag = currentDateSupplier.getLocalDate();

		var spec = heeftProject(project)
			.and(heeftActieveClient())
			.and(heeftExcludeerAfmelding(project.getExcludeerAfmelding()))
			.and(isNietInProjectBrief(definitie))
			.and(heeftActieveClientInProjectVoorProjectClient(vandaag));

		return projectClientRepository.findAll(spec);
	}

	@Override
	public List<Project> getAllProjectenWhereProjectBriefActieHasBriefType(BriefType type)
	{
		var vandaag = currentDateSupplier.getLocalDate();

		return projectRepository.findWith(heeftBriefTypeInProject(type)
			.and(heeftEindDatumNaVandaag(vandaag)), q -> q.distinct(true).all());
	}

	@Override
	public List<ProjectBrief> getAllProjectBriefForHerinnering(ProjectBriefActie actie, Date verstuurdOp)
	{

		var vandaag = currentDateSupplier.getLocalDate();

		var spec = heeftActieveClientInProjectVoorProjectBrief(vandaag)
			.and(heeftDefinitieVragenlijst())
			.and(heeftPrintDatumNaOfOpDatum(verstuurdOp))
			.and(heeftDefinitieGelijkAanBaseActie(actie))
			.and(heeftVragenlijstAntwoordenStatusNullOfNietAfgerond())
			.and(heeftGeenVerstuurdeBrief(actie));
		return projectBriefRepository.findAll(spec);
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
	public String generateVragenlijstUrl(ProjectBrief projectBrief)
	{
		var sb = new StringBuilder(applicationUrl);
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
		var sb = new StringBuilder("B");
		sb.append(projectBrief.getId().toString());
		var controleGetal = projectBrief.getProjectClient().getClient().getId().toString();
		sb.append(controleGetal.substring(controleGetal.length() - 4));
		for (var i = sb.length() - 4; i > 0; i = i - 4)
		{
			sb.insert(i, '-');
		}
		return sb.toString();
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
	public ProjectBriefActie getProjectBriefActie(Client client, BriefType briefType)
	{
		var vandaag = currentDateSupplier.getLocalDate();

		var spec = ProjectBriefActieSpecification.heeftBriefTypeInProjectBriefActie(briefType)
			.and(isProjectBriefActieTypeVervangendeBrief())
			.and(heeftClient(client))
			.and(heeftActieveClientInProjectVoorProjectBriefActie(vandaag));

		return projectBriefActieRepository.findFirst(spec, Sort.by(Sort.Direction.DESC, AbstractHibernateObject_.ID)).orElse(null);
	}

	@Override
	public Long getAantalProjectClientenVanProject(Project project)
	{
		return projectDao.getAantalProjectClientenVanProject(project);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public boolean addVragenlijstAanTemplate(MailMergeContext context, Document chunkDocument, ProjectBriefActie actie, ProjectBrief projectBrief) throws Exception
	{
		var vragenlijst = actie.getVragenlijst();
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
					var vragenlijstTemplate = uploadDocumentService.load(vragenlijstFormulierInstantie.getTemplateVanGebruiker());
					var vragenlijstTemplateBytes = FileUtils.readFileToByteArray(vragenlijstTemplate);
					vragenlijstDocument = asposeService.processDocument(vragenlijstTemplateBytes, context);
				}
			}
			if (vragenlijstDocument != null)
			{
				chunkDocument.getLastSection().getHeadersFooters().linkToPrevious(false);
				chunkDocument.appendDocument(vragenlijstDocument, ImportFormatMode.KEEP_SOURCE_FORMATTING);

				var holder = new ProjectVragenlijstAntwoordenHolder();
				holder.setStatus(ProjectVragenlijstStatus.AANGEMAAKT);
				holder.setVragenlijst(vragenlijst);

				var antwoorden = new VragenlijstAntwoorden<ProjectVragenlijstAntwoordenHolder>();
				antwoorden.setFormulierInstantie(vragenlijstFormulierInstantie);
				antwoorden.setAntwoordenHolder(holder);
				holder.setVragenlijstAntwoorden(antwoorden);
				var resultaat = new FormulierResultaatImpl();
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
	public void updateWachtOpStartProject(Bevolkingsonderzoek bvo)
	{
		projectDao.resetWachtOpStartProject(bvo);
		projectDao.setNieuwWachtOpStartProject(bvo, currentDateSupplier.getDate());
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED, noRollbackFor = IllegalStateException.class)
	public ProjectClient addClientToProjectGroep(ProjectGroep groep, Client client)
	{
		valideerRulesPopulatie(groep, client);

		var projectClient = new ProjectClient();
		projectClient.setGroep(groep);
		projectClient.setProject(groep.getProject());
		projectClient.setClient(client);
		projectClient.setToegevoegd(currentDateSupplier.getDate());
		client.getProjecten().add(projectClient);

		groep.setPopulatie(groep.getPopulatie() + 1);
		groep.getClienten().add(projectClient);
		hibernateService.saveOrUpdateAll(groep, projectClient);
		return projectClient;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
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
	@Transactional(propagation = Propagation.REQUIRED)
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
}
