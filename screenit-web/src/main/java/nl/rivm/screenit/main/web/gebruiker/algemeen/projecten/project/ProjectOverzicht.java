
package nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.project;

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

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.NaamChoiceRenderer;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitListMultipleChoice;
import nl.rivm.screenit.main.web.component.form.FilterBvoPanel;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.algemeen.AlgemeenPage;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.base.ZoekenContextMenuItem;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.INaam;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectStatus;
import nl.rivm.screenit.model.project.ProjectType;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.ProjectService;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Button;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@ZoekenContextMenuItem
@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_PROJECT_OVERZICHT, Recht.GEBRUIKER_BRIEFPROJECT_OVERZICHT },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class ProjectOverzicht extends AlgemeenPage
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private ProjectService projectService;

	private IModel<Project> projectZoekModel;

	private WebMarkupContainer projectContainer;

	private ToegangLevel toegangLevelProjectOverzicht, toegangLevelBriefprojectOverzicht;

	public ProjectOverzicht()
	{
		if (ScreenitSession.get().isZoekObjectGezetForComponent(ProjectOverzicht.class))
		{
			projectZoekModel = (IModel<Project>) ScreenitSession.get().getZoekObject(ProjectOverzicht.class);
		}
		else
		{
			projectZoekModel = ModelUtil.cRModel(new Project());
		}

		toegangLevelProjectOverzicht = getToegangsLevel(Actie.INZIEN, Recht.GEBRUIKER_PROJECT_OVERZICHT);
		toegangLevelBriefprojectOverzicht = getToegangsLevel(Actie.INZIEN, Recht.GEBRUIKER_BRIEFPROJECT_OVERZICHT);

		addNieuwProjectDropDown();

		setDefaultModel(new CompoundPropertyModel<>(projectZoekModel));
		ScreenitForm<Project> form = new ScreenitForm<Project>("form", (IModel<Project>) getDefaultModel());
		add(form);

		form.add(new FilterBvoPanel<Project>("bvoFilterContainer", form.getModel(), false));

		form.add(new IndicatingAjaxSubmitLink("filteren", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				ScreenitSession.get().setZoekObject(ProjectOverzicht.class, form.getModel());
				WebMarkupContainer container = getProjectenTable();
				projectContainer.replaceWith(container);
				projectContainer = container;
				target.add(projectContainer);
			}
		});

		form.add(new ScreenitListMultipleChoice<ProjectStatus>("projectStatussen", Arrays.asList(ProjectStatus.values()), new NaamChoiceRenderer<INaam>()));

		form.add(new ScreenitListMultipleChoice<ProjectType>("projectTypes", Arrays.asList(ProjectType.values()), new EnumChoiceRenderer<ProjectType>())
			.setVisible(toegangLevelProjectOverzicht != null && toegangLevelBriefprojectOverzicht != null));

		projectContainer = getProjectenTable();
		add(projectContainer);
	}

	private void addNieuwProjectDropDown()
	{
		List<ProjectType> projectTypesMetRecht = projectService.getProjectTypes(ScreenitSession.get().getLoggedInInstellingGebruiker(), Actie.TOEVOEGEN, true);
		ListView<ProjectType> nieuwProjectTypes = new ListView<ProjectType>("nieuwProjectTypes", projectTypesMetRecht)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(final ListItem<ProjectType> item)
			{
				AjaxLink<Void> nieuwProject = new AjaxLink<Void>("nieuwProject")
				{

					private static final long serialVersionUID = 1L;

					@Override
					public void onClick(AjaxRequestTarget target)
					{
						Project project = new Project();
						project.setType(item.getModelObject());
						setResponsePage(new ProjectEditPage(ModelUtil.cModel(project)));
					}

				};
				item.add(nieuwProject);

				nieuwProject.add(new EnumLabel<ProjectType>("label", item.getModelObject()));
			}

		};
		add(nieuwProjectTypes);

		WebMarkupContainer caret = new WebMarkupContainer("caret");
		caret.setVisible(!projectTypesMetRecht.isEmpty() && projectTypesMetRecht.size() > 1);

		Label toevoegenDropdownTekst;
		if (!projectTypesMetRecht.isEmpty() && projectTypesMetRecht.size() == 1 && projectTypesMetRecht.get(0).equals(ProjectType.BRIEFPROJECT))
		{
			toevoegenDropdownTekst = new Label("toevoegenDropdownTekst", getString("briefproject.toevoegen"));
		}
		else
		{
			toevoegenDropdownTekst = new Label("toevoegenDropdownTekst", getString("project.toevoegen"));
		}
		toevoegenDropdownTekst.setVisible(!projectTypesMetRecht.isEmpty());

		if (!projectTypesMetRecht.isEmpty() && projectTypesMetRecht.size() == 1)
		{
			nieuwProjectTypes.setVisible(false);

			IndicatingAjaxLink<Project> dropDownKnop = new IndicatingAjaxLink<Project>("toevoegenDropdown", ModelUtil.cModel(new Project()))
			{

				private static final long serialVersionUID = 1L;

				@Override
				public void onClick(AjaxRequestTarget target)
				{
					Project project = new Project();
					project.setType(projectTypesMetRecht.get(0));
					setResponsePage(new ProjectEditPage(ModelUtil.cModel(project)));
				}
			};
			dropDownKnop.setVisible(projectTypesMetRecht.size() > 0);
			dropDownKnop.add(toevoegenDropdownTekst);
			dropDownKnop.add(caret);
			add(dropDownKnop);
		}
		else
		{
			Button toevoegen = new Button("toevoegenDropdown");
			toevoegen.add(toevoegenDropdownTekst);
			toevoegen.add(caret);

			toevoegen.setVisible(!projectTypesMetRecht.isEmpty());
			nieuwProjectTypes.setVisible(!projectTypesMetRecht.isEmpty());

			add(toevoegen);
		}
	}

	private WebMarkupContainer getProjectenTable()
	{
		WebMarkupContainer container = new WebMarkupContainer("projectenContainer");
		container.setOutputMarkupId(true);
		SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy");

		List<IColumn<Project, String>> columns = new ArrayList<IColumn<Project, String>>();
		columns.add(new AbstractColumn<Project, String>(Model.of("Bvo"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<Project>> cellItem, String componentId, IModel<Project> rowModel)
			{
				cellItem.add(new Label(componentId, Model.of(Bevolkingsonderzoek.getAfkortingen(rowModel.getObject().getBevolkingsonderzoeken()))));
			}

		});
		columns.add(new PropertyColumn<Project, String>(Model.of("Naam"), "naam", "naam"));
		columns.add(new AbstractColumn<Project, String>(Model.of("SO"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<Project>> cellItem, String componentId, IModel<Project> rowModel)
			{
				StringBuilder soLabel = new StringBuilder();
				rowModel.getObject().getScreeningOrganisaties();
				for (Instelling instelling : rowModel.getObject().getScreeningOrganisaties())
				{
					soLabel.append(instelling.getNaam());
					soLabel.append(",");
				}
				cellItem.add(new Label(componentId, StringUtils.removeEnd(soLabel.toString(), ",")));
			}

		});
		columns.add(new PropertyColumn<Project, String>(Model.of("Organisatie"), "organisatie.naam", "organisatie"));
		columns.add(new PropertyColumn<Project, String>(Model.of("Contactpersoon"), "contactpersoon.medewerker", "contactpersoon.medewerker.naamVolledig"));
		columns.add(new DateTimePropertyColumn<Project, String>(Model.of("Startdatum"), "startDatum", "startDatum", format));
		columns.add(new DateTimePropertyColumn<Project, String>(Model.of("Einddatum instroom"), "eindeInstroom", "eindeInstroom", format));
		columns.add(new DateTimePropertyColumn<Project, String>(Model.of("Eindedatum"), "eindDatum", "eindDatum", format));
		columns.add(new EnumPropertyColumn<Project, String, ProjectType>(Model.of("Projecttype"), "type"));
		columns.add(new AbstractColumn<Project, String>(Model.of("Status"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<Project>> cellItem, String componentId, IModel<Project> rowModel)
			{
				Date vandaag = currentDateSupplier.getDate();
				cellItem.add(new EnumLabel<ProjectStatus>(componentId, ProjectUtil.getStatus(rowModel.getObject(), vandaag)));
			}

		});

		Map<ProjectType, ToegangLevel> toegangLevelHashMap = new HashMap<ProjectType, ToegangLevel>();
		toegangLevelHashMap.put(ProjectType.PROJECT, toegangLevelProjectOverzicht);
		toegangLevelHashMap.put(ProjectType.BRIEFPROJECT, toegangLevelBriefprojectOverzicht);

		ScreenitDataTable<Project, String> dataTable = new ScreenitDataTable<Project, String>("projecten", columns,
			new ProjectDataProvider(projectZoekModel, toegangLevelHashMap), 10, new Model<String>("Projecten"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target, IModel<Project> model)
			{
				setResponsePage(new ProjectStatusPage(model));
			}
		};

		dataTable.setOutputMarkupId(true);
		container.add(dataTable);
		return container;
	}

	private ToegangLevel getToegangsLevel(Actie actie, Recht recht)
	{
		return autorisatieService.getToegangLevel(ScreenitSession.get().getLoggedInInstellingGebruiker(), actie, true, recht);
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<GebruikerMenuItem>();
		contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.projecten.overzicht", ProjectOverzicht.class));
		return contextMenuItems;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(projectZoekModel);
	}

}
