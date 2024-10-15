package nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.bestanden;

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

import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.ProjectBasePage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.ProjectPaspoortPanel;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectBestand;
import nl.rivm.screenit.model.project.ProjectBestandType;
import nl.rivm.screenit.model.project.ProjectBestandVerwerking;
import nl.rivm.screenit.model.project.ProjectBestandVerwerkingEntry;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

import static nl.rivm.screenit.model.project.ProjectBestandVerwerkingEntry_.MELDING;
import static nl.rivm.screenit.model.project.ProjectBestandVerwerkingEntry_.REGEL_NUMMER;

public class ProjectBestandVerwerkingPage extends ProjectBasePage
{
	private static final long serialVersionUID = 1L;

	private final IModel<ProjectBestand> bestandModel;

	public ProjectBestandVerwerkingPage(IModel<Project> model, IModel<ProjectBestand> bestandModel)
	{
		super(model);
		setDefaultModel(ModelUtil.ccModel(bestandModel.getObject().getVerwerking()));
		this.bestandModel = bestandModel;

		add(new Label("bestandsNaam", bestandModel.getObject().getUploadDocument().getNaam()));

		add(getPassPoortContainer());

		add(getStatistiekenContainer());

		add(getFoutenContainer());
	}

	private WebMarkupContainer getStatistiekenContainer()
	{
		WebMarkupContainer container = new WebMarkupContainer("statistiekenContainer");
		container.setOutputMarkupId(true);

		container.add(new Label("projectBestand.uploadDocument.naam"));
		container.add(new Label("regelsVerwerkt"));
		container.add(new Label("regelsMislukt"));

		container.add(new WebMarkupContainer("attributenGevondenLabel").setVisible(!bestandModel.getObject().getType().equals(ProjectBestandType.UITSLAGEN)));
		container.add(new Label("attributenGevonden"));
		container.add(DateLabel.forDatePattern("projectBestand.uploadDatum", "dd-MM-yyyy HH:mm:ss"));
		container.add(new EnumLabel<BestandStatus>("projectBestand.status"));
		container.add(getToegepastOpLabel());
		ProjectBestandType projectBestandType = bestandModel.getObject().getType();
		container.add(new Label("projectBestand.dynamischeInactiveerReden")
			.setVisible(ProjectBestandType.INACTIVEREN.equals(projectBestandType) || ProjectBestandType.HERACTIVEREN.equals(projectBestandType)));

		return container;
	}

	private Label getToegepastOpLabel()
	{
		ProjectBestand bestand = bestandModel.getObject();
		ProjectGroep groep = bestand.getGroep();
		if (groep != null)
		{
			return new Label("toegepastOp", Model.of(groep.getNaam()));
		}
		return new Label("toegepastOp", Model.of("Hele project"));
	}

	private WebMarkupContainer getFoutenContainer()
	{
		WebMarkupContainer container = new WebMarkupContainer("meldingenContainer");
		container.setOutputMarkupId(true);

		List<IColumn<ProjectBestandVerwerkingEntry, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<>(Model.of("Regelnummer"), REGEL_NUMMER, REGEL_NUMMER));
		columns.add(new PropertyColumn<>(Model.of("Reden regel niet verwerkt"), MELDING, MELDING));

		ScreenitDataTable<ProjectBestandVerwerkingEntry, String> dataTable = new ScreenitDataTable<>("meldingen", columns,
			new ProjectBestandVerwerkingDataProvider((IModel<ProjectBestandVerwerking>) getDefaultModel()), 10, Model.of("Meldingen"))
		{
			@Override
			protected boolean isRowClickable(IModel<ProjectBestandVerwerkingEntry> rowModel)
			{
				return false;
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
		ModelUtil.nullSafeDetach(bestandModel);
	}

	@Override
	protected boolean isDefaultModelProjectModel()
	{
		return false;
	}

}
