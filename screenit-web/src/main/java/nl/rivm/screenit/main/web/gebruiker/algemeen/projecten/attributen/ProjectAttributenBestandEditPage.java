package nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.attributen;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.service.algemeen.ProjectService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.ProjectBasePage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.ProjectPaspoortPanel;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.bestanden.ProjectBestandenOverzicht;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectBestand;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
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

public class ProjectAttributenBestandEditPage extends ProjectBasePage
{
	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private ProjectService projectService;

	@SpringBean
	private UploadDocumentService uploadDocumentService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private LogService logService;

	private final IModel<ProjectBestand> bestandModel;

	private final IModel<List<FileUpload>> bestanden = new ListModel<>();

	public ProjectAttributenBestandEditPage(IModel<Project> model)
	{
		super(model);
		bestandModel = ModelUtil.ccModel(new ProjectBestand());
		ProjectBestand bestand = bestandModel.getObject();
		bestand.setProject(model.getObject());
		Form<ProjectBestand> form = new Form<>("form", bestandModel);
		add(form);

		FormComponent<List<FileUpload>> bestandUpload = new FileUploadField("bestand", bestanden).add(new FileValidator(FileType.CSV));
		bestandUpload.setRequired(true);
		form.add(bestandUpload);
		form.add(new ScreenitDropdown<>("toepassenOp", new ListModel<>(getOptionsVoorToepassenOp())).setRequired(true));
		form.add(ComponentHelper.newCheckBox("attributen").setRequired(true));

		add(getPassPoortContainer());
		addButtons(form);
	}

	private List<String> getOptionsVoorToepassenOp()
	{
		Project project = getProjectModel().getObject();
		List<String> options = new ArrayList<String>();
		options.add("Hele project");
		for (ProjectGroep groep : project.getGroepen())
		{
			options.add(groep.getNaam());
		}
		return options;
	}

	private ProjectGroep getGroepVanToepassenOp(Project project, String waarde)
	{
		for (ProjectGroep groep : project.getGroepen())
		{
			if (groep.getNaam().equals(waarde))
			{
				return groep;
			}
		}
		return null;
	}

	private void addButtons(Form<ProjectBestand> form)
	{
		add(new IndicatingAjaxSubmitLink("opslaan", form)
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				ProjectBestand bestand = form.getModelObject();
				Project project = getProjectModel().getObject();
				if (!bestand.isAttributen())
				{
					error("'Attributen' is verplicht.");
					return;
				}
				if (bestanden.getObject().size() == 1)
				{
					try
					{
						FileUpload attributenBestand = bestanden.getObject().get(0);
						projectService.queueProjectBestandVoorAttributen(project, getGroepVanToepassenOp(getProjectModel().getObject(), bestand.getToepassenOp()), bestand,
							attributenBestand.getContentType(),
							attributenBestand.getClientFileName(),
							attributenBestand.writeToTempFile(), ScreenitSession.get().getLoggedInAccount());

						setResponsePage(new ProjectBestandenOverzicht(getProjectModel()));

					}
					catch (Exception e)
					{
						error("Bestand kon niet worden geimporteerd.");
					}
				}
				else
				{
					error("Meerdere bestanden gevonden met de upload, kan maar 1 tegelijk uploaden");
				}
			}

		});

		add(new IndicatingAjaxLink<Void>("annuleren")
		{
			@Override
			public void onClick(AjaxRequestTarget ajaxRequestTarget)
			{
				setResponsePage(new ProjectAttributenPage(getProjectModel()));
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
