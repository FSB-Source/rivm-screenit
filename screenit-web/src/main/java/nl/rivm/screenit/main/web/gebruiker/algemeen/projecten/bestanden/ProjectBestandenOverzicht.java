package nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.bestanden;

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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.ProjectBasePage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.ProjectPaspoortPanel;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectBestand;
import nl.rivm.screenit.model.project.ProjectBestandType;
import nl.rivm.screenit.model.project.ProjectBestandVerwerking;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.search.column.BooleanStringPropertyColumn;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public class ProjectBestandenOverzicht extends ProjectBasePage
{

	private static final long serialVersionUID = 1L;

	private WebMarkupContainer bestandenContainer;

	private IModel<ProjectBestand> filterModel;

	private IModel<Project> projectModel;

	public ProjectBestandenOverzicht(IModel<Project> model)
	{
		super(model);
		projectModel = model;
		filterModel = ModelUtil.cModel(new ProjectBestand());
		ProjectBestand bestand = filterModel.getObject();
		bestand.setProject(model.getObject());

		bestandenContainer = getBestandenContainer();
		add(bestandenContainer);

		add(getPassPoortContainer());
	}

	private WebMarkupContainer getBestandenContainer()
	{
		WebMarkupContainer container = new WebMarkupContainer("projectBestandenContainer");
		container.setOutputMarkupId(true);

		SimpleDateFormat format = Constants.getDateTimeSecondsFormat();

		List<IColumn<ProjectBestand, String>> columns = new ArrayList<IColumn<ProjectBestand, String>>();
		columns.add(new PropertyColumn<ProjectBestand, String>(Model.of("Naam"), "uploadDocument.naam", "uploadDocument.naam"));
		columns.add(new EnumPropertyColumn<ProjectBestand, String, ProjectBestandType>(Model.of("Type"), "type", "type"));
		columns.add(new AbstractColumn<ProjectBestand, String>(Model.of("Toepassen op"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<ProjectBestand>> cellItem, String componentId, IModel<ProjectBestand> rowModel)
			{
				String waarde = "Hele project";
				ProjectBestand bestand = ModelUtil.nullSafeGet(rowModel);
				if (bestand.getGroep() != null)
				{
					waarde = bestand.getGroep().getNaam();
				}
				cellItem.add(new Label(componentId, Model.of(waarde)));
			}
		});
		Map<Boolean, String> presentatie = new HashMap<>();
		presentatie.put(Boolean.TRUE, "Ja");
		presentatie.put(Boolean.FALSE, "Nee");
		columns.add(new BooleanStringPropertyColumn<ProjectBestand, String>(Model.of("Attributen"), presentatie, "attributen"));
		columns.add(new DateTimePropertyColumn<ProjectBestand, String>(Model.of("Uploaddatum"), "uploadDatum", "uploadDatum", format));
		columns.add(new AbstractColumn<ProjectBestand, String>(Model.of("Regels mislukt"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<ProjectBestand>> cellItem, String componentId, IModel<ProjectBestand> rowModel)
			{
				String value = "0";
				ProjectBestand bestand = rowModel.getObject();
				ProjectBestandVerwerking verwerking = bestand.getVerwerking();
				if (verwerking != null)
				{
					value = verwerking.getRegelsMislukt() + "";
				}
				cellItem.add(new Label(componentId, Model.of(value)));
			}
		});
		columns.add(new AbstractColumn<ProjectBestand, String>(Model.of("Regels verwerkt"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<ProjectBestand>> cellItem, String componentId, IModel<ProjectBestand> rowModel)
			{
				String value = "0";
				ProjectBestand bestand = rowModel.getObject();
				ProjectBestandVerwerking verwerking = bestand.getVerwerking();
				if (verwerking != null)
				{
					value = verwerking.getRegelsVerwerkt() + "";
				}
				cellItem.add(new Label(componentId, Model.of(value)));
			}
		});
		columns.add(new AbstractColumn<ProjectBestand, String>(Model.of("Status"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<ProjectBestand>> cellItem, String componentId, IModel<ProjectBestand> rowModel)
			{
				ProjectBestand bestand = rowModel.getObject();
				String status = bestand.getStatus().getNaam();
				Label label = new Label(componentId, Model.of(status));
				cellItem.add(label);
			}
		});

		ScreenitDataTable<ProjectBestand, String> dataTable = new ScreenitDataTable<ProjectBestand, String>("projectBestanden", columns,
			new ProjectBestandenDataProvider(filterModel), 10, Model.of("bestanden"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target, IModel<ProjectBestand> model)
			{
				setResponsePage(new ProjectBestandVerwerkingPage(getProjectModel(), model));
			}

			@Override
			protected boolean isRowClickable(IModel<ProjectBestand> rowModel)
			{
				ProjectBestand bestand = rowModel.getObject();
				return BestandStatus.NOG_TE_VERWERKEN != bestand.getStatus();
			}
		};
		container.add(dataTable);

		return container;
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
