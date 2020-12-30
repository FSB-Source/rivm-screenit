package nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.attributen;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.table.ActiefPropertyColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.ProjectBasePage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.ProjectPaspoortPanel;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectAttribuut;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.search.column.BooleanStringPropertyColumn;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ProjectAttributenPage extends ProjectBasePage
{
	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private LogService logService;

	private WebMarkupContainer attributenContainer;

	private final BootstrapDialog dialog;

	private IModel<ProjectAttribuut> filterModel;

	private IModel<Project> projectModel;

	public ProjectAttributenPage(IModel<Project> model)
	{
		super(model);
		projectModel = model;
		filterModel = ModelUtil.cModel(new ProjectAttribuut());
		filterModel.getObject().setProject(model.getObject());

		dialog = new BootstrapDialog("dialog");
		add(dialog);

		add(new IndicatingAjaxLink<Object>("attribuutToevoegen")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{

				IModel<ProjectAttribuut> projectAttribuutModel = ModelUtil.cModel(new ProjectAttribuut());
				projectAttribuutModel.getObject().setProject(projectModel.getObject());

				openProjectAttribuutEditPage(target, projectAttribuutModel);
			}

		}.setVisible(magAttributenBewerken()));

		add(new IndicatingAjaxLink<Void>("bestandToevoegen")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(new ProjectAttributenBestandEditPage(model));
			}

		}.setVisible(magPopulatieBewerken()));

		attributenContainer = getAttributenContainer();
		add(attributenContainer);

		add(getPassPoortContainer());
	}

	private boolean magAttributenBewerken()
	{
		return getToegangsLevel(getRechtOverzicht(), Actie.AANPASSEN) != null;
	}

	private boolean magPopulatieBewerken()
	{
		return getToegangsLevel(getRechtSelectie(), Actie.AANPASSEN) != null;
	}

	private WebMarkupContainer getAttributenContainer()
	{
		WebMarkupContainer container = new WebMarkupContainer("projectAttributenContainer");
		container.setOutputMarkupId(true);

		List<IColumn<ProjectAttribuut, String>> columns = new ArrayList<IColumn<ProjectAttribuut, String>>();
		columns.add(new PropertyColumn<>(Model.of("Naam"), "naam", "naam"));
		columns.add(new PropertyColumn<>(Model.of("Mergefield"), "mergeField", "mergeField"));

		Map<Boolean, String> booleanStringMap = new HashMap<Boolean, String>();
		booleanStringMap.put(true, "Niet zichtbaar");
		booleanStringMap.put(false, "Zichtbaar");
		columns.add(new BooleanStringPropertyColumn<>(Model.of("Zichtbaarheid in cliëntdossier"), booleanStringMap, "nietZichtbaarInClientDossier"));
		columns.add(new ActiefPropertyColumn<ProjectAttribuut, ProjectAttribuut>(Model.of("Verwijderen"), "actief", container, filterModel, true, dialog,
			"ProjectAttributenPage.verwijderen")
		{

			@Override
			protected void onAfterToggleActief(AjaxRequestTarget target, ProjectAttribuut actiefObject)
			{
				hibernateService.saveOrUpdate(actiefObject);

				WebMarkupContainer container = getAttributenContainer();
				ProjectAttributenPage.this.attributenContainer.replaceWith(container);
				ProjectAttributenPage.this.attributenContainer = container;
				target.add(ProjectAttributenPage.this.attributenContainer);

				Project project = actiefObject.getProject();
				String melding = "Project attribuut" + actiefObject.getNaam() + " ge\u00EFnactiveerd van Project " + project.getNaam();

				logService.logGebeurtenis(LogGebeurtenis.PROJECT_ATTRIBUUT_VERWIJDERD, ScreenitSession.get().getLoggedInAccount(), melding);

				info("Project attribuut succesvol ge\u00EFnactiveerd!");
			}

			@Override
			protected boolean mayToggle(ProjectAttribuut actiefObject)
			{
				return actiefObject.getActief() && magAttributenBewerken();
			}
		});

		ScreenitDataTable<ProjectAttribuut, String> dataTable = new ScreenitDataTable<ProjectAttribuut, String>("projectAttributen", columns,
			new ProjectAttributenDataProvider(filterModel), 10, Model.of("attributen"))
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<ProjectAttribuut> model)
			{
				openProjectAttribuutEditPage(target, model);
			}

			@Override
			protected boolean isRowClickable(IModel<ProjectAttribuut> rowModel)
			{
				return magAttributenBewerken();
			}
		};
		container.add(dataTable);
		return container;
	}

	private void openProjectAttribuutEditPage(AjaxRequestTarget target, IModel<ProjectAttribuut> attribuutModel)
	{
		dialog.openWith(target, new ProjectAttribuutEditPanel(BootstrapDialog.CONTENT_ID, attribuutModel)
		{
			@Override
			protected void opslaan(AjaxRequestTarget target)
			{
				WebMarkupContainer container = getAttributenContainer();
				ProjectAttributenPage.this.attributenContainer.replaceWith(container);
				ProjectAttributenPage.this.attributenContainer = container;
				target.add(ProjectAttributenPage.this.attributenContainer);

				dialog.close(target);
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

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(filterModel);
		ModelUtil.nullSafeDetach(projectModel);
	}
}
