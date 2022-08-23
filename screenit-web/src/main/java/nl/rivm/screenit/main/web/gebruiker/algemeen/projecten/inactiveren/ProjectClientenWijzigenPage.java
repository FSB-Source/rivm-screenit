package nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.inactiveren;

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
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.ConfirmPanel;
import nl.rivm.screenit.main.web.component.modal.DefaultConfirmCallback;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.ProjectBasePage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.ProjectPaspoortPanel;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.bestanden.ProjectBestandenOverzicht;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectBestand;
import nl.rivm.screenit.model.project.ProjectBestandType;
import nl.rivm.screenit.service.ProjectService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@Slf4j
@SecurityConstraint(
	actie = Actie.VERWIJDEREN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_PROJECT_SELECTIE,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.MAMMA })
public class ProjectClientenWijzigenPage extends ProjectBasePage
{
	@SpringBean
	private ProjectService projectService;

	@SpringBean
	private UploadDocumentService uploadDocumentService;

	@SpringBean
	private HibernateService hibernateService;

	private final IModel<List<FileUpload>> clientenBestanden = new ListModel<>();

	private final IModel<ProjectBestand> formModel;

	private IModel<UploadDocument> documentModel;

	private final BootstrapDialog dialog;

	private final Form<ProjectBestand> form;

	public ProjectClientenWijzigenPage(IModel<Project> model)
	{
		super(model);

		formModel = ModelUtil.ccModel(new ProjectBestand());

		form = new Form<>("form", formModel);
		add(form);

		dialog = new BootstrapDialog("dialog");
		add(dialog);

		FormComponent<String> inactiveerReden = ComponentHelper.addTextField(form, "dynamischeInactiveerReden", false, 255, false);
		inactiveerReden.setLabel(Model.of("Inactiveer reden"));

		FormComponent<List<FileUpload>> clientenBestand = new FileUploadField("bestand", clientenBestanden)
			.add(new FileValidator(FileType.CSV));
		clientenBestand.setRequired(true);
		clientenBestand.setLabel(Model.of("Bestand met clienten"));
		form.add(clientenBestand);

		List<ProjectBestandType> projectBestandTypes = new ArrayList<>();
		projectBestandTypes.add(ProjectBestandType.INACTIVEREN);
		projectBestandTypes.add(ProjectBestandType.HERACTIVEREN);
		projectBestandTypes.add(ProjectBestandType.VERWIJDEREN);

		RadioChoice<ProjectBestandType> bestandTypeRadio = new RadioChoice<>("type", projectBestandTypes,
			new EnumChoiceRenderer<>(this));
		bestandTypeRadio.setPrefix("<label class=\"radio\" style=\"padding-left:5px\">");
		bestandTypeRadio.setSuffix("</label>");
		bestandTypeRadio.setOutputMarkupId(true);
		bestandTypeRadio.setRequired(true);
		form.add(bestandTypeRadio);

		add(new AjaxSubmitLink("inactiveren", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				if (clientenBestanden.getObject().size() == 1)
				{
					stelFileVeilig(); 

					ProjectBestand projectBestand = (ProjectBestand) form.getModelObject();
					String dialogTekst = "Weet u zeker dat u deze clienten wilt " + getString("ProjectBestandType." + projectBestand.getType() + ".melding") + "?";
					dialog.openWith(target, new ConfirmPanel(dialog.CONTENT_ID, Model.of(dialogTekst), null, new DefaultConfirmCallback()
					{
						@Override
						public void onYesClick(AjaxRequestTarget target)
						{
							try
							{
								Project project = getProjectModel().getObject();

								ProjectBestand projectBestand = ProjectClientenWijzigenPage.this.form.getModelObject();
								UploadDocument uploadDocument = documentModel.getObject();

								projectService.queueProjectBestandVoorClientWijzigingen(project, projectBestand, uploadDocument, uploadDocument.getContentType(),
									uploadDocument.getNaam(),
									uploadDocument.getFile(), ScreenitSession.get().getLoggedInAccount());

								setResponsePage(new ProjectBestandenOverzicht(getProjectModel()));
							}
							catch (Exception e)
							{
								LOG.error("Fout", e);
								error("Bestand kon niet worden geimporteerd.");
							}
						}

					}, dialog));

				}
				else
				{
					error("Meerdere bestanden gevonden met de upload, kan maar 1 tegelijk uploaden");
				}
			}

		});
		WebMarkupContainer passpoortContainer = getPassPoortContainer();

		add(passpoortContainer);
	}

	private void stelFileVeilig()
	{
		try
		{
			if (clientenBestanden.getObject() != null)
			{
				FileUpload upload = clientenBestanden.getObject().get(0);
				File definitieFile = upload.writeToTempFile();

				UploadDocument document = new UploadDocument();
				document.setActief(Boolean.TRUE);
				document.setContentType(upload.getContentType());
				document.setFile(definitieFile);
				document.setNaam(upload.getClientFileName());
				this.documentModel = ModelUtil.ccModel(document);
			}
		}
		catch (Exception e)
		{
			LOG.error("Fout bij uploaden van een inactiveren-/heractiverenbestand naar tmp directory: ", e);
			error(getString("error.onbekend"));
			return;
		}
	}

	private WebMarkupContainer getPassPoortContainer()
	{
		WebMarkupContainer container = new WebMarkupContainer("projectPasspoortContainer");
		container.setOutputMarkupId(true);

		container.add(new ProjectPaspoortPanel("projectPasspoort", getProjectModel()));

		return container;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(formModel);
		ModelUtil.nullSafeDetach(documentModel);
	}
}
