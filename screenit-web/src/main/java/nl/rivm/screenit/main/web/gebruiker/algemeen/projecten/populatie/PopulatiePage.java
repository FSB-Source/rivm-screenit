
package nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.populatie;

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

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.main.service.algemeen.ProjectService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.table.ActiefPropertyColumn;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.component.table.VerwijderPropertyColumn;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.ProjectBasePage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.ProjectPaspoortPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.project.GroepInvoer;
import nl.rivm.screenit.model.project.GroepSelectieType;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

import static nl.rivm.screenit.model.project.ProjectGroep_.ACTIEF;
import static nl.rivm.screenit.model.project.ProjectGroep_.GROEP_INVOER;
import static nl.rivm.screenit.model.project.ProjectGroep_.NAAM;
import static nl.rivm.screenit.model.project.ProjectGroep_.POPULATIE;
import static nl.rivm.screenit.model.project.ProjectGroep_.UITNODIGINGEN_PUSHEN_NA;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_PROJECT_SELECTIE, Recht.GEBRUIKER_BRIEFPROJECT_SELECTIE },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.MAMMA })
public class PopulatiePage extends ProjectBasePage
{
	private final BootstrapDialog dialog;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private ProjectService projectService;

	private WebMarkupContainer groepenContainer;

	private final IModel<ProjectGroep> zoekModel;

	public PopulatiePage(IModel<Project> model)
	{
		super(model);

		zoekModel = ModelUtil.ccModel(new ProjectGroep());
		zoekModel.getObject().setProject(model.getObject());

		if (!ScreenitSession.get().isZoekObjectGezetForComponent(PopulatiePage.class))
		{
			ScreenitSession.get().setZoekObject(PopulatiePage.class, zoekModel);
		}

		dialog = new BootstrapDialog("dialog");
		add(dialog);

		add(new AjaxLink<Void>("groepToevoegen")
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(new ProjectGroepEditPage(getProjectModel()));
			}

			@Override
			public void onConfigure()
			{
				super.onConfigure();
				Project project = getProjectModel().getObject();
				Date eindDatum = project.getEindDatum();
				Date nu = currentDateSupplier.getDate();
				boolean levelProject = getToegangsLevel(Recht.GEBRUIKER_PROJECT_SELECTIE, Actie.TOEVOEGEN) != null;
				boolean levelBriefproject = getToegangsLevel(Recht.GEBRUIKER_BRIEFPROJECT_SELECTIE, Actie.TOEVOEGEN) != null;
				setVisible(!eindDatum.before(nu) && (levelProject || levelBriefproject) && project.getGroepSelectieType() == GroepSelectieType.STATISCH);
			}
		});

		groepenContainer = getGroepenDataTable();
		add(groepenContainer);
		add(new ProjectPaspoortPanel("projectPasspoort", model));
	}

	private WebMarkupContainer getGroepenDataTable()
	{
		WebMarkupContainer groepenContainer = new WebMarkupContainer("groepenContainer");
		groepenContainer.setOutputMarkupId(true);

		List<IColumn<ProjectGroep, String>> columns = new ArrayList<IColumn<ProjectGroep, String>>();
		columns.add(new PropertyColumn<>(Model.of("Naam"), NAAM));
		columns.add(new PropertyColumn<>(Model.of("Populatie"), POPULATIE));
		columns.add(new AbstractColumn<>(Model.of("Inactief"))
		{
			@Override
			public void populateItem(Item<ICellPopulator<ProjectGroep>> cellItem, String componentId, IModel<ProjectGroep> rowModel)
			{
				Long inactieveProjectClienten = projectService.getAantalInactieveProjectClientenVanProjectGroep(rowModel.getObject());
				cellItem.add(new Label(componentId, Model.of(inactieveProjectClienten)));
			}
		});
		SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy");
		columns.add(new DateTimePropertyColumn<>(Model.of("Push"), UITNODIGINGEN_PUSHEN_NA, UITNODIGINGEN_PUSHEN_NA, format));
		columns.add(new EnumPropertyColumn<ProjectGroep, String, GroepInvoer>(Model.of("Type"), GROEP_INVOER));
		columns
			.add(new ActiefPropertyColumn<>(Model.of("Actief"), ACTIEF, groepenContainer, zoekModel, true, dialog, "question.project.inactiveer.groep")
			{

				@Override
				protected void onAfterToggleActief(AjaxRequestTarget target, ProjectGroep actiefObject)
				{
					String melding = projectService.updateProjectGroepActiefStatus(actiefObject, ScreenitSession.get().getLoggedInAccount());
					info(melding);

					WebMarkupContainer container = getGroepenDataTable();
					PopulatiePage.this.groepenContainer.replaceWith(container);
					PopulatiePage.this.groepenContainer = container;
					target.add(PopulatiePage.this.groepenContainer);
				}
			});

		if (getToegangsLevel(Recht.GEBRUIKER_PROJECT_SELECTIE, Actie.VERWIJDEREN) != null)
		{
			VerwijderPropertyColumn verwijderPropertyColumn = new VerwijderPropertyColumn<ProjectGroep, String>(Model.of("Verwijderen"), getString("verwijderen.vervangendeTekst"),
				dialog, getString("verwijderen.dialogMessage"))
			{
				@Override
				public void onClickDeleteAction(AjaxRequestTarget target, IModel<ProjectGroep> rowModel)
				{
					projectService.verwijderProjectGroep(ModelProxyHelper.deproxy(rowModel.getObject()), ScreenitSession.get().getLoggedInAccount());

					WebMarkupContainer container = getGroepenDataTable();
					PopulatiePage.this.groepenContainer.replaceWith(container);
					PopulatiePage.this.groepenContainer = container;
					target.add(PopulatiePage.this.groepenContainer);
				}

				@Override
				public boolean isButtonVisible(IModel<ProjectGroep> rowModel)
				{
					return rowModel.getObject().getActiefDatum() == null;
				}
			};
			columns.add(verwijderPropertyColumn);
		}

		ScreenitDataTable<ProjectGroep, String> dataTable = new ScreenitDataTable<>("groepen", columns, new PopulatieDataProvider(zoekModel), 10,
			Model.of("Groepen"))
		{

			@Override
			public void onClick(AjaxRequestTarget target, IModel<ProjectGroep> model)
			{
				setResponsePage(new ProjectGroepEditPage(ModelUtil.ccModel(model.getObject()), (IModel<Project>) PopulatiePage.this.getDefaultModel()));
			}
		};
		groepenContainer.add(dataTable);

		return groepenContainer;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(zoekModel);
	}
}
