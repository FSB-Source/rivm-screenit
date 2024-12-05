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

import java.util.Date;
import java.util.List;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Root;

import nl.rivm.screenit.dao.ProjectDao;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.ProjectParameter_;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.formulieren.ScreenitFormulierInstantie;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectAttribuut;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectClientAttribuut;
import nl.rivm.screenit.model.project.ProjectClient_;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.rivm.screenit.model.project.ProjectGroep_;
import nl.rivm.screenit.model.project.ProjectType;
import nl.rivm.screenit.model.project.ProjectVragenlijstAntwoordenHolder;
import nl.rivm.screenit.model.project.ProjectVragenlijstStatus;
import nl.rivm.screenit.model.project.ProjectVragenlijstUitzettenVia;
import nl.rivm.screenit.model.project.Project_;
import nl.rivm.screenit.model.vragenlijsten.VragenlijstAntwoorden;
import nl.rivm.screenit.repository.algemeen.ProjectBriefActieRepository;
import nl.rivm.screenit.repository.algemeen.ProjectBriefRepository;
import nl.rivm.screenit.repository.algemeen.ProjectClientAttribuutRepository;
import nl.rivm.screenit.repository.algemeen.ProjectClientRepository;
import nl.rivm.screenit.repository.algemeen.ProjectGroepRepository;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.BaseProjectService;
import nl.rivm.screenit.service.ClientDoelgroepService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.specification.algemeen.ProjectBriefActieSpecification;
import nl.rivm.screenit.specification.algemeen.ProjectGroepSpecification;
import nl.rivm.screenit.util.AfmeldingUtil;
import nl.rivm.screenit.util.BezwaarUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.formulieren2.persistence.resultaat.FormulierResultaatImpl;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.io.FileUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.aspose.words.Document;
import com.aspose.words.ImportFormatMode;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
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
import static nl.rivm.screenit.specification.algemeen.ProjectClientSpecification.heeftActieveProjectGroep;
import static nl.rivm.screenit.specification.algemeen.ProjectClientSpecification.heeftExcludeerAfmelding;
import static nl.rivm.screenit.specification.algemeen.ProjectClientSpecification.heeftProject;
import static nl.rivm.screenit.specification.algemeen.ProjectClientSpecification.isNietInProjectBrief;
import static nl.rivm.screenit.specification.algemeen.ProjectClientSpecification.isProjectClientActief;
import static nl.rivm.screenit.specification.algemeen.ProjectSpecification.isActiefOpDatum;

@Service
public class BaseProjectServiceImpl implements BaseProjectService
{
	private static final String VRAGENLIJST_PREFIX = "vragenlijst/";

	@Autowired
	private ProjectDao projectDao;

	@Autowired
	private ClientService clientService;

	@Autowired
	private ClientDoelgroepService doelgroepService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired
	private AsposeService asposeService;

	@Autowired
	private String applicationUrl;

	@Autowired
	private ProjectClientRepository projectClientRepository;

	@Autowired
	private ProjectBriefRepository projectBriefRepository;

	@Autowired
	private ProjectBriefActieRepository projectBriefActieRepository;

	@Autowired
	private ProjectClientAttribuutRepository projectClientAttribuutRepository;

	@Autowired
	private ProjectGroepRepository projectGroepRepository;

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
		if (!excludeerAfmeldingOnderzoeken.isEmpty())
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
			else if (getProjectClient(client, project) != null)
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
	public void updateWachtOpStartProject(Bevolkingsonderzoek bvo)
	{
		projectDao.resetWachtOpStartProject(bvo);
		projectDao.setNieuwWachtOpStartProject(bvo, currentDateSupplier.getDate());
	}

	@Override
	public List<ProjectClient> getValideClientenVanProject(Project project, ProjectBriefActie definitie)
	{
		var vandaag = currentDateSupplier.getLocalDate();

		var spec = heeftProject(project)
			.and(heeftActieveClient())
			.and(heeftExcludeerAfmelding(project.getExcludeerAfmelding()))
			.and(isNietInProjectBrief(definitie))
			.and(isActiefOpDatum(vandaag).with(q ->
			{
				var groepJoin = join(q, ProjectClient_.groep);
				return join(groepJoin, ProjectGroep_.project);
			}))
			.and(isProjectClientActief(true))
			.and(heeftActieveProjectGroep());

		return projectClientRepository.findAll(spec);
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
	@Transactional(propagation = Propagation.REQUIRED)
	public boolean addVragenlijstAanTemplate(MailMergeContext context, Document chunkDocument, ProjectBriefActie actie, ProjectBrief projectBrief) throws Exception
	{
		var vragenlijst = actie.getVragenlijst();
		if (vragenlijst != null && ProjectVragenlijstUitzettenVia.isPapier(actie.getProjectVragenlijstUitzettenVia()))
		{
			Document vragenlijstDocument = null;
			ScreenitFormulierInstantie vragenlijstFormulierInstantie;
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
	public List<ProjectGroep> getActieveProjectGroepenVoorUitnodigingDK()
	{
		var sort = Sort.by(
			Sort.Order.desc(ProjectGroep_.PROJECT + "." + Project_.PARAMETERS + "." + ProjectParameter_.VALUE),
			Sort.Order.asc(ProjectGroep_.PROJECT + "." + Project_.START_DATUM),
			Sort.Order.asc(ProjectGroep_.PROJECT + "." + AbstractHibernateObject_.ID),
			Sort.Order.asc(ProjectGroep_.ACTIEF_DATUM),
			Sort.Order.asc(ProjectGroep_.UITNODIGEN_VOOR_DKVOOR),
			Sort.Order.asc(AbstractHibernateObject_.ID));
		return projectGroepRepository.findWith(ProjectGroepSpecification.getActieveProjectGroepenVoorUitnodigingDK(currentDateSupplier.getDate()), ProjectGroep.class,
			q -> q.sortBy(sort, BaseProjectServiceImpl::addJoinsForSortingOrCreateDedicatedOrders)).all();
	}

	private static Order addJoinsForSortingOrCreateDedicatedOrders(Sort.Order order, Root<ProjectGroep> r, CriteriaBuilder cb)
	{
		var sortProperty = order.getProperty();
		if (sortProperty.startsWith(ProjectGroep_.PROJECT + "." + Project_.PARAMETERS + "." + ProjectParameter_.VALUE))
		{
			var project = join(r, ProjectGroep_.project);
			var parameters = join(project, Project_.parameters);
			var value = parameters.get(ProjectParameter_.value).as(Integer.class);
			return order.isAscending() ? cb.asc(value) : cb.desc(value);
		}
		return null;
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
	public ProjectClientAttribuut getProjectClientAttribuut(ProjectClient client, ProjectAttribuut attribuut)
	{
		return projectClientAttribuutRepository.findOneByProjectClientAndAttribuut(client, attribuut).orElse(null);
	}

	@Override
	public ProjectClient getProjectClient(Client client, Project project)
	{
		return projectClientRepository.findOneByProjectAndClient(project, client).orElse(null);
	}

}
