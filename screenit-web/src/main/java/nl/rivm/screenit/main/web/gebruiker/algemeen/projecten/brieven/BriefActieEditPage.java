package nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.brieven;

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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.ProjectBasePage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.ProjectPaspoortPanel;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectBriefActieType;
import nl.rivm.screenit.model.project.ProjectType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.bouncycastle.util.Strings;

@Slf4j
public class BriefActieEditPage extends ProjectBasePage
{
	@SpringBean
	private UploadDocumentService uploadDocumentService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private LogService logService;

	private final Form<ProjectBriefActie> form;

	private WebMarkupContainer typePanelContainer;

	private final IModel<ProjectBriefActie> briefActieModel;

	private final IModel<ProjectBriefActie> briefHerinnerenVragenlijstModel;

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

		var types = new ArrayList<ProjectBriefActieType>();
		types.add(ProjectBriefActieType.DATUM);
		types.add(ProjectBriefActieType.VANAF_DATUM);
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
			}
		});
		form.add(typeDropDown);

		form.add(new TextField<>("printomschrijving"));

		add(new IndicatingAjaxLink<>("annuleren", model)
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(new ProjectBriefActiePage(getModel()));
			}
		});

		add(new IndicatingAjaxSubmitLink("opslaan", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				ProjectBriefActie actie = form.getModelObject();
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
						uploadDocument = ScreenitSession.get().fileUploadToUploadDocument(filesUploaded.get(0));
						uploadDocumentService.saveOrUpdate(uploadDocument, FileStoreLocation.PROJECT_BRIEF_TEMPLATES, actie.getProject().getId());
						actie.setDocument(uploadDocument);
						actie.setUploader(ScreenitSession.get().getLoggedInInstellingGebruiker());
						actie.setLaatstGewijzigd(nu);
						hibernateService.saveOrUpdate(actie);

						if (actie.isHerinneren())
						{
							herinnerActie = briefHerinnerenVragenlijstModel.getObject();
							herinneringsDocument = ScreenitSession.get().fileUploadToUploadDocument(herinneringFilesUpload.get(0));
							uploadDocumentService.saveOrUpdate(herinneringsDocument, FileStoreLocation.PROJECT_BRIEF_TEMPLATES, actie.getProject().getId());
							herinnerActie.setUploader(ScreenitSession.get().getLoggedInInstellingGebruiker());
							herinnerActie.setDocument(herinneringsDocument);
							herinnerActie.setLaatstGewijzigd(nu);
							herinnerActie.setType(ProjectBriefActieType.HERINNERING);
							herinnerActie.setActief(true);
							hibernateService.saveOrUpdate(herinnerActie);
							actie.setHerinneringsActie(herinnerActie);
							hibernateService.saveOrUpdate(actie);
						}

						String melding = getString(EnumStringUtil.getPropertyString(project.getType())) + ": " + project.getNaam() +
							" Briefsoort: " + getString(EnumStringUtil.getPropertyString(actie.getType()));

						if (actie.getBriefType() != null)
						{
							melding += actie.getBriefType().getWeergaveNaam();
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
		if (type != null)
		{
			panel = switch (type)
			{
				case DATUM, VANAF_DATUM -> new BriefActieTypeDatumPanel("typePanel", briefActieModel);
				case VERVANGENDEBRIEF -> new BriefActieTypeVervangendeBriefPanel("typePanel", briefActieModel);
				case HERINNERING -> new BriefActieTypeHerinneringPanel("typePanel", briefActieModel);
				case XDAGENNAY -> new BriefActieTypeXnaYPanel("typePanel", briefActieModel);
				case XMETY -> new BriefActieTypeXmetYPanel("typePanel", briefActieModel);
				default -> new EmptyPanel("typePanel");
			};
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
		Instelling organisatie = ScreenitSession.get().getLoggedInInstellingGebruiker().getOrganisatie();
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
