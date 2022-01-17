package nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.brieven;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.main.comparator.ProjectVragenlijstComparator;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.NaamChoiceRenderer;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.ProjectBasePage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.ProjectPaspoortPanel;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectBriefActieType;
import nl.rivm.screenit.model.project.ProjectType;
import nl.rivm.screenit.model.project.ProjectVragenlijst;
import nl.rivm.screenit.model.project.ProjectVragenlijstUitzettenVia;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.VragenlijstBaseService;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.organisatie.model.Organisatie;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.SimpleListHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.RangeValidator;
import org.bouncycastle.util.Strings;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class BriefActieEditPage extends ProjectBasePage
{
	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(BriefActieEditPage.class);

	@SpringBean
	private FileService fileService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private VragenlijstBaseService vragenlijstBaseService;

	private Form<ProjectBriefActie> form;

	private WebMarkupContainer typePanelContainer;

	private WebMarkupContainer vragenlijstContainer;

	private WebMarkupContainer vragenlijstHerinnerenContainer;

	private IModel<ProjectBriefActie> briefActieModel;

	private IModel<ProjectBriefActie> briefHerinnerenVragenlijstModel = null;

	private final IModel<List<FileUpload>> fileUploads = new ListModel<>();

	private final IModel<List<FileUpload>> herinnerFileUploads = new ListModel<>();

	public BriefActieEditPage(IModel<Project> model)
	{
		super(model);
		Project project = model.getObject();
		ProjectBriefActie actie = new ProjectBriefActie();
		briefActieModel = ModelUtil.ccModel(actie);
		briefActieModel.getObject().setProject(project);

		ProjectBriefActie herinnerActie = new ProjectBriefActie();
		briefHerinnerenVragenlijstModel = ModelUtil.ccModel(herinnerActie);
		briefHerinnerenVragenlijstModel.getObject().setProject(project);

		form = new Form<>("form", briefActieModel);
		form.setMultiPart(true);
		add(form);

		typePanelContainer = getTypePanelContainer(null);
		form.add(typePanelContainer);

		final WebMarkupContainer fileUploadContainer = new WebMarkupContainer("fileUploadContainer");
		fileUploadContainer.setVisible(false);
		fileUploadContainer.setOutputMarkupPlaceholderTag(true);
		FileUploadField upload = new FileUploadField("fileUpload", fileUploads);
		upload.setRequired(true);
		upload.add(new FileValidator(FileType.WORD_NIEUW));
		fileUploadContainer.add(upload);
		form.add(fileUploadContainer);

		vragenlijstContainer = getVragenlijstContainer();
		form.add(vragenlijstContainer);

		List<ProjectBriefActieType> types = new ArrayList<ProjectBriefActieType>();

		types.add(ProjectBriefActieType.DATUM);
		types.add(ProjectBriefActieType.XDAGENNAY);
		types.add(ProjectBriefActieType.XMETY);
		if (ProjectType.PROJECT.equals(project.getType()))
		{
			types.add(ProjectBriefActieType.VERVANGENDEBRIEF);
		}

		ScreenitDropdown<ProjectBriefActieType> typeDropDown = new ScreenitDropdown<>("type", new ListModel<>(types),
			new EnumChoiceRenderer<>());
		typeDropDown.add(new AjaxFormComponentUpdatingBehavior("change")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				ProjectBriefActie actie = form.getModelObject();
				WebMarkupContainer container = getTypePanelContainer(actie.getType());
				typePanelContainer.replaceWith(container);
				typePanelContainer = container;
				target.add(typePanelContainer);
				fileUploadContainer.setVisible(actie.getType() != null);
				target.add(fileUploadContainer);

				container = getVragenlijstContainer();
				vragenlijstContainer.replaceWith(container);
				vragenlijstContainer = container;
				target.add(vragenlijstContainer);
				target.add(vragenlijstContainer);
			}
		});
		form.add(typeDropDown);

		form.add(new TextField<>("printomschrijving"));

		add(new IndicatingAjaxLink<Project>("annuleren", model)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(new ProjectBriefActiePage(getModel()));
			}
		});

		add(new IndicatingAjaxSubmitLink("opslaan", form)
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				ProjectBriefActie actie = (ProjectBriefActie) form.getModelObject();
				Project project = actie.getProject();
				if (validatieProjectBriefActie(actie))
				{
					ProjectBriefActie herinnerActie;
					UploadDocument uploadDocument;
					UploadDocument herinneringsDocument;
					Date nu = currentDateSupplier.getDate();
					List<FileUpload> filesUploaded = fileUploads.getObject();
					List<FileUpload> herinneringFilesUpload = herinnerFileUploads.getObject();
					try
					{
						FileUpload fileUpload = filesUploaded.get(0);
						File file = filesUploaded.get(0).writeToTempFile();
						uploadDocument = new UploadDocument();
						uploadDocument.setFile(file);
						uploadDocument.setNaam(fileUpload.getClientFileName());
						uploadDocument.setContentType(fileUpload.getContentType());
						uploadDocument.setActief(true);
						fileService.saveOrUpdateUploadDocument(uploadDocument, FileStoreLocation.PROJECT_BRIEF_TEMPLATES, actie.getProject().getId());
						actie.setDocument(uploadDocument);
						actie.setUploader(ScreenitSession.get().getLoggedInInstellingGebruiker());
						actie.setLaatstGewijzigd(nu);
						hibernateService.saveOrUpdate(actie);

						if (actie.isHerinneren())
						{
							herinnerActie = briefHerinnerenVragenlijstModel.getObject();
							fileUpload = herinneringFilesUpload.get(0);
							file = fileUpload.writeToTempFile();
							herinneringsDocument = new UploadDocument();
							herinneringsDocument.setFile(file);
							herinneringsDocument.setNaam(fileUpload.getClientFileName());
							herinneringsDocument.setContentType(fileUpload.getContentType());
							herinneringsDocument.setActief(true);
							fileService.saveOrUpdateUploadDocument(herinneringsDocument, FileStoreLocation.PROJECT_BRIEF_TEMPLATES, actie.getProject().getId());
							herinnerActie.setUploader(ScreenitSession.get().getLoggedInInstellingGebruiker());
							herinnerActie.setDocument(herinneringsDocument);
							herinnerActie.setLaatstGewijzigd(nu);
							herinnerActie.setType(ProjectBriefActieType.HERINNERING);
							herinnerActie.setProjectVragenlijstUitzettenVia(actie.getProjectVragenlijstUitzettenVia());
							herinnerActie.setActief(true);
							hibernateService.saveOrUpdate(herinnerActie);
							actie.setHerinneringsActie(herinnerActie);
							hibernateService.saveOrUpdate(actie);
						}

						String melding = getString(EnumStringUtil.getPropertyString(project.getType())) + ": " + project.getNaam() +
							" Briefsoort: " + getString(EnumStringUtil.getPropertyString(actie.getType()));

						if (actie.getBriefType() != null)
						{
							melding += getString(EnumStringUtil.getPropertyString(actie.getBriefType()));
						}

						if (ProjectType.BRIEFPROJECT.equals(project.getType()))
						{
							logService.logGebeurtenis(LogGebeurtenis.BRIEFPROJECT_BRIEF_ACTIE_TOEGEVOEGD, ScreenitSession.get().getLoggedInAccount(), melding);
						}
						else
						{
							logService.logGebeurtenis(LogGebeurtenis.PROJECT_BRIEF_ACTIE_TOEGEVOEGD, ScreenitSession.get().getLoggedInAccount(), melding);
						}

						setResponsePage(new ProjectBriefActiePage(ModelUtil.sModel(project)));
					}
					catch (Exception e)
					{

						LOG.debug("Kon file niet wegschrijven naar temp", e);
						ScreenitSession.get().error("Fout bij opslaan van bestand (" + e.getMessage() + ")");
					}
				}
			}
		});

		add(new ProjectPaspoortPanel("projectPasspoort", model));

	}

	private boolean validatieProjectBriefActie(ProjectBriefActie actie)
	{
		if (isZelfdeBriefActieAlAanwezig())
		{
			error("Er is al een " + Strings.toLowerCase(getString("ProjectBriefActieType." + actie.getType())) + " voor dat brieftype.");
			return false;
		}
		if (CollectionUtils.isEmpty(fileUploads.getObject()))
		{
			error("Er is geen definitie geupload.");
			return false;
		}
		if (actie.isHerinneren() && CollectionUtils.isEmpty(herinnerFileUploads.getObject()))
		{
			error("Er is geen herinnerings definitie geupload.");
			return false;
		}
		if (getBestandsNaam(actie).length() > 255)
		{
			error("De bestandsnaam zal te lang worden met deze printomschrijving.");
			return false;
		}

		return true;
	}

	private WebMarkupContainer getVragenlijstContainer()
	{
		WebMarkupContainer vragenlijstContainer = new WebMarkupContainer("vragenlijstContainer");
		vragenlijstContainer.setVisible(getProjectModel().getObject().getType().equals(ProjectType.PROJECT));
		vragenlijstContainer.setOutputMarkupPlaceholderTag(true);

		vragenlijstHerinnerenContainer = getVragenlijstHerinnerenContainer();
		vragenlijstContainer.add(vragenlijstHerinnerenContainer);

		final WebMarkupContainer vragenlijstInfoContainer = new WebMarkupContainer("vragenlijstInfoContainer");
		vragenlijstInfoContainer.setVisible(briefActieModel.getObject().getVragenlijst() != null);
		vragenlijstInfoContainer.setOutputMarkupPlaceholderTag(true);
		vragenlijstContainer.add(vragenlijstInfoContainer);

		ScreenitDropdown<ProjectVragenlijstUitzettenVia> uitzettenVia = new ScreenitDropdown<>("projectVragenlijstUitzettenVia",
			new ListModel<>(Collections.singletonList(ProjectVragenlijstUitzettenVia.WEB)), new EnumChoiceRenderer<>());
		uitzettenVia.setRequired(true);
		uitzettenVia.setNullValid(true);
		uitzettenVia.setLabel(Model.of("Uitzetten via"));
		vragenlijstInfoContainer.add(uitzettenVia);

		ProjectVragenlijst projectVragenlijst = new ProjectVragenlijst();
		projectVragenlijst.setActief(true);
		List<ProjectVragenlijst> vragenlijsten = vragenlijstBaseService.searchVragenlijsten(projectVragenlijst, 0L, -1L, "naam", true);
		ProjectVragenlijst huidigeVragenlijst = form.getModelObject().getVragenlijst();
		if (huidigeVragenlijst != null && huidigeVragenlijst.getId() != null && !vragenlijsten.contains(huidigeVragenlijst))
		{
			vragenlijsten.add(huidigeVragenlijst);
		}

		vragenlijsten.sort(new ProjectVragenlijstComparator());
		SimpleListHibernateModel<ProjectVragenlijst> projectVragenlijsten = new SimpleListHibernateModel<>(vragenlijsten);

		ScreenitDropdown<ProjectVragenlijst> vragenlijst = new ScreenitDropdown<>("vragenlijst", projectVragenlijsten, new NaamChoiceRenderer<>());
		vragenlijst.setNullValid(true);
		vragenlijst.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				vragenlijstInfoContainer.setVisible(briefActieModel.getObject().getVragenlijst() != null);
				target.add(vragenlijstInfoContainer);
				WebMarkupContainer container = getVragenlijstHerinnerenContainer();
				vragenlijstHerinnerenContainer.replaceWith(container);
				vragenlijstHerinnerenContainer = container;
				target.add(vragenlijstHerinnerenContainer);
			}
		});
		vragenlijstContainer.add(vragenlijst);
		return vragenlijstContainer;
	}

	private WebMarkupContainer getVragenlijstHerinnerenContainer()
	{
		WebMarkupContainer vragenlijstHerinnerenContainer = new WebMarkupContainer("vragenlijstHerinnerenContainer");
		vragenlijstHerinnerenContainer.setVisible(briefActieModel.getObject().getVragenlijst() != null);
		vragenlijstHerinnerenContainer.setOutputMarkupPlaceholderTag(true);

		final WebMarkupContainer vragenlijstHerinnerenInfoContainer = new WebMarkupContainer("herinnerenInfoContainer");
		vragenlijstHerinnerenInfoContainer.setVisible(briefActieModel.getObject().isHerinneren());
		vragenlijstHerinnerenInfoContainer.setOutputMarkupPlaceholderTag(true);
		vragenlijstHerinnerenContainer.add(vragenlijstHerinnerenInfoContainer);

		CheckBox checkHerinneren = ComponentHelper.newCheckBox("herinneren");
		vragenlijstHerinnerenContainer.add(checkHerinneren);

		TextField<Integer> aantalDagenText = new TextField<Integer>("aantalDagen", new PropertyModel<>(briefHerinnerenVragenlijstModel, "aantalDagen"), Integer.class);
		aantalDagenText.add(RangeValidator.minimum(1));
		aantalDagenText.setLabel(Model.of("Herinnering aantal dagen"));
		vragenlijstHerinnerenInfoContainer.add(aantalDagenText);

		FileUploadField upload = new FileUploadField("herinnerUpload", herinnerFileUploads);
		upload.setRequired(true);
		upload.setLabel(Model.of("Herinnering template"));
		upload.add(new FileValidator(FileType.WORD_NIEUW));
		vragenlijstHerinnerenInfoContainer.add(upload);

		checkHerinneren.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				vragenlijstHerinnerenInfoContainer.setVisible(briefActieModel.getObject().isHerinneren());
				target.add(vragenlijstHerinnerenInfoContainer);
			}
		});
		return vragenlijstHerinnerenContainer;

	}

	private WebMarkupContainer getTypePanelContainer(ProjectBriefActieType type)
	{
		WebMarkupContainer container = new WebMarkupContainer("typePanelContainer");
		container.setOutputMarkupId(true);

		container.add(getTypePanel(type));
		return container;
	}

	private Panel getTypePanel(ProjectBriefActieType type)
	{
		Panel panel;
		if (ProjectBriefActieType.DATUM.equals(type))
		{
			panel = new BriefActieTypeDatumPanel("typePanel", briefActieModel);
		}
		else if (ProjectBriefActieType.VERVANGENDEBRIEF.equals(type))
		{
			panel = new BriefActieTypeVervangendeBriefPanel("typePanel", briefActieModel);
		}
		else if (ProjectBriefActieType.HERINNERING.equals(type))
		{
			panel = new BriefActieTypeHerinneringPanel("typePanel", briefActieModel);
		}
		else if (ProjectBriefActieType.XDAGENNAY.equals(type))
		{
			panel = new BriefActieTypeXnaYPanel("typePanel", briefActieModel);
		}
		else if (ProjectBriefActieType.XMETY.equals(type))
		{
			panel = new BriefActieTypeXmetYPanel("typePanel", briefActieModel);
		}
		else
		{
			panel = new EmptyPanel("typePanel");
		}
		panel.setOutputMarkupPlaceholderTag(true);
		return panel;
	}

	private boolean isZelfdeBriefActieAlAanwezig()
	{
		ProjectBriefActie nieuweActie = briefActieModel.getObject();
		List<ProjectBriefActie> acties = nieuweActie.getProject().getProjectBriefActies();
		for (ProjectBriefActie actie : acties)
		{
			boolean isGelijkAanVervangendeBrief = ProjectBriefActieType.VERVANGENDEBRIEF.equals(nieuweActie.getType())
				&& ProjectBriefActieType.VERVANGENDEBRIEF.equals(actie.getType()) && nieuweActie.getBriefType().equals(actie.getBriefType())
				&& Boolean.TRUE.equals(actie.getActief());
			boolean isGelijkAanXDAGENNAY = ProjectBriefActieType.XDAGENNAY.equals(nieuweActie.getType()) && ProjectBriefActieType.XDAGENNAY.equals(actie.getType())
				&& nieuweActie.getBriefType().equals(actie.getBriefType()) && nieuweActie.getAantalDagen().equals(actie.getAantalDagen()) && Boolean.TRUE.equals(actie.getActief());
			boolean isGelijkAanXMETY = ProjectBriefActieType.XMETY.equals(nieuweActie.getType()) && ProjectBriefActieType.XMETY.equals(actie.getType())
				&& nieuweActie.getBriefType().equals(actie.getBriefType()) && Boolean.TRUE.equals(actie.getActief());
			boolean isGelijkAanDatum = ProjectBriefActieType.DATUM.equals(nieuweActie.getType()) && ProjectBriefActieType.DATUM.equals(actie.getType())
				&& nieuweActie.getDatum().compareTo(actie.getDatum()) == 0 && Boolean.TRUE.equals(actie.getActief());
			if (isGelijkAanVervangendeBrief || isGelijkAanXDAGENNAY || isGelijkAanDatum || isGelijkAanXMETY)
			{
				return true;
			}
		}
		return false;
	}

	private String getBestandsNaam(ProjectBriefActie actie)
	{
		String naam = "2017-01-01_12.00-";
		Organisatie organisatie = ScreenitSession.get().getLoggedInInstellingGebruiker().getOrganisatie();
		if (organisatie != null)
		{
			String soNaam = organisatie.getNaam();
			soNaam = soNaam.replaceAll(" ", "_");
			naam += soNaam + "-";
		}
		if (actie.getProject().getNaam() != null)
		{
			String projectNaam = actie.getProject().getNaam();
			projectNaam = projectNaam.replaceAll(" ", "_");
			naam += projectNaam + "-";
		}
		if (StringUtils.isNotEmpty(actie.getPrintomschrijving()))
		{
			naam += actie.getPrintomschrijving().replace(" ", "_").toLowerCase();
		}
		return naam + ".pdf";
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(briefActieModel);
		ModelUtil.nullSafeDetach(fileUploads);
		ModelUtil.nullSafeDetach(briefHerinnerenVragenlijstModel);
	}
}
