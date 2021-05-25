package nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.uitslag;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.ProjectBasePage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.ProjectPaspoortPanel;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.bestanden.ProjectBestandenOverzicht;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.project.ProjectStatusPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectBestand;
import nl.rivm.screenit.service.ProjectService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_UITSLAG_PROJECT_UPLOADEN,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class ProjectUitslagUploadenPage extends ProjectBasePage
{
	@SpringBean
	private ProjectService projectService;

	private IModel<ProjectBestand> uitslagModel;

	private IModel<List<FileUpload>> uitslagen = new ListModel<>();

	public ProjectUitslagUploadenPage(IModel<Project> model)
	{
		super(model);

		uitslagModel = ModelUtil.ccModel(new ProjectBestand());
		ProjectBestand uitslag = uitslagModel.getObject();
		uitslag.setProject(model.getObject());
		Form<ProjectBestand> form = new Form<>("form", uitslagModel);
		add(form);

		FormComponent<List<FileUpload>> uitslagUpload = new FileUploadField("uitslag", uitslagen).add(new FileValidator(FileType.CSV));
		uitslagUpload.setRequired(true);
		form.add(uitslagUpload);

		add(getPassPoortContainer());
		addButtons(form);

	}

	private void addButtons(Form<ProjectBestand> form)
	{
		add(new IndicatingAjaxSubmitLink("toevoegen", form)
		{

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				ProjectBestand uitslag = form.getModelObject();
				Project project = getProjectModel().getObject();
				if (uitslagen.getObject().size() == 1)
				{
					try
					{
						FileUpload uitslagenBestand = uitslagen.getObject().get(0);
						projectService.queueProjectBestandVoorUitslagen(project, uitslag,
							uitslagenBestand.getContentType(),
							uitslagenBestand.getClientFileName(),
							uitslagenBestand.writeToTempFile(), ScreenitSession.get().getLoggedInAccount());

						setResponsePage(new ProjectBestandenOverzicht(getProjectModel()));
					}
					catch (Exception e)
					{
						error("Bestand kon niet worden geimporteerd.");
					}
				}
				else
				{
					error("Meerdere bestanden gevonden met de upload, kan maar 1 tegelijk uploaden.");
				}
			}
		});

		add(new IndicatingAjaxLink<Void>("annuleren")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(new ProjectStatusPage(getProjectModel()));
			}
		});
	}

	private WebMarkupContainer getPassPoortContainer()
	{
		WebMarkupContainer container = new WebMarkupContainer("projectPasspoortContainer");
		container.setOutputMarkupId(true);

		container.add(new ProjectPaspoortPanel("projectPasspoort", getProjectModel()));

		return container;
	}
}
