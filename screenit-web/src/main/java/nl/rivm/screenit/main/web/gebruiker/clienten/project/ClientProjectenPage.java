
package nl.rivm.screenit.main.web.gebruiker.clienten.project;

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

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientPage;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientPaspoortPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.rivm.screenit.model.project.ProjectStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
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
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.GEBRUIKER_CLIENT_GEGEVENS, bevolkingsonderzoekScopes = {
	Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class ClientProjectenPage extends ClientPage
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	private IModel<ProjectClient> projectClientModel;

	private SimpleDateFormat formatter = new SimpleDateFormat("dd-MM-yyyy");

	public ClientProjectenPage(IModel<Client> client)
	{
		super(client);

		projectClientModel = ModelUtil.ccModel(new ProjectClient());
		if (ScreenitSession.get().isZoekObjectGezetForComponent(ClientProjectenPage.class))
		{
			projectClientModel = (IModel<ProjectClient>) ScreenitSession.get().getZoekObject(ClientProjectenPage.class);
		}
		ProjectClient projectClient = projectClientModel.getObject();
		projectClient.setClient(client.getObject());

		add(new ClientPaspoortPanel("paspoort", client));

		add(getProjectenDataTable());
	}

	private WebMarkupContainer getProjectenDataTable()
	{
		WebMarkupContainer container = new WebMarkupContainer("projectenContainer");
		container.setOutputMarkupId(true);

		List<IColumn<ProjectClient, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<ProjectClient, String>(Model.of("Naam"), "project.naam", "project.naam"));
		columns.add(new AbstractColumn<ProjectClient, String>(Model.of("Projectstatus"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<ProjectClient>> cellItem, String componentId, IModel<ProjectClient> rowModel)
			{
				Date nu = currentDateSupplier.getDate();
				ProjectStatus status = ProjectUtil.getStatus(rowModel.getObject().getProject(), nu);
				cellItem.add(new Label(componentId, Model.of(getString(EnumStringUtil.getPropertyString(status)))));
			}

		});
		columns.add(new AbstractColumn<ProjectClient, String>(Model.of("Clientstatus"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<ProjectClient>> cellItem, String componentId, IModel<ProjectClient> rowModel)
			{
				String status = "Actief";
				ProjectClient projectClient = rowModel.getObject();
				ProjectGroep projectGroep = projectClient.getGroep();
				if (!projectClient.getActief() || !projectGroep.getActief())
				{
					status = "Inactief";
				}
				cellItem.add(new Label(componentId, Model.of(status)));

			}
		});
		columns.add(new DateTimePropertyColumn<ProjectClient, String>(Model.of("Startdatum"), "project.startDatum", "project.startDatum", formatter));

		columns.add(new DateTimePropertyColumn<ProjectClient, String>(Model.of("Einddatum instroom"), "project.eindeInstroom", "project.eindeInstroom", formatter));
		columns.add(new DateTimePropertyColumn<ProjectClient, String>(Model.of("Einddatum"), "project.eindDatum", "project.eindDatum", formatter));

		ScreenitDataTable<ProjectClient, String> dataTable = new ScreenitDataTable<ProjectClient, String>("projecten", columns, new ClientProjectenDataProvider(projectClientModel),
			10, new Model<String>("Projecten"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target, IModel<ProjectClient> model)
			{
				setResponsePage(new ClientProjectGegevens(model));
			}

			@Override
			protected boolean getValuesFromSession()
			{
				return false;
			}
		};
		container.add(dataTable);
		return container;

	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(projectClientModel);
	}
}
