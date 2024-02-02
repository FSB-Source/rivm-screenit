package nl.rivm.screenit.main.web.gebruiker.clienten.complicatie;

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
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenissen;
import nl.rivm.screenit.main.service.ClientDossierFilter;
import nl.rivm.screenit.main.service.DossierService;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.Complicatie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.ComplicatieErnst;
import nl.rivm.screenit.model.enums.ComplicatieSoort;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.model.DetachableListModel;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Fragment;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ClientComplicatieOverzichtPanel extends GenericPanel<Client>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private DossierService dossierService;

	private IModel<Complicatie> complicatieModel;

	private List<String> addedTooltips = new ArrayList<>();

	public ClientComplicatieOverzichtPanel(String id, IModel<Client> model)
	{
		super(id, model);

		ClientDossierFilter clientDossierFilter = new ClientDossierFilter(Arrays.asList(Bevolkingsonderzoek.values()), false);

		final IModel<List<ScreeningRondeGebeurtenissen>> dossierModel = new DetachableListModel<>(
			dossierService.getScreeningRondeGebeurtenissen(getModelObject(), clientDossierFilter));

		List<IColumn<Complicatie, String>> columns = new ArrayList<>();
		columns.add(new AbstractColumn<Complicatie, String>(Model.of("Ronde"), "rondeNr")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<Complicatie>> cellItem, String componentId, IModel<Complicatie> rowModel)
			{
				Integer rondeNr = 0;
				ColonScreeningRonde screeningRonde = rowModel.getObject().getClient().getColonDossier().getLaatsteScreeningRonde();
				List<ScreeningRondeGebeurtenissen> screeningRondeGebeurtenissens = dossierModel.getObject();
				if (rowModel.getObject().getMdlverslag() != null && rowModel.getObject().getMdlverslag().getScreeningRonde() != null)
				{
					screeningRonde = rowModel.getObject().getMdlverslag().getScreeningRonde();
				}

				for (ScreeningRondeGebeurtenissen rondeDossier : screeningRondeGebeurtenissens)
				{
					if (rondeDossier.getScreeningRonde().equals(screeningRonde))
					{
						rondeNr = rondeDossier.getRondenr();
					}
				}
				Label label = new Label(componentId, rondeNr.toString());
				label.add(new AttributeAppender("class", Model.of("badge")));
				cellItem.add(label);
			}
		});
		columns.add(new DateTimePropertyColumn<Complicatie, String>(Model.of("Datum"), "datum", "datum", new SimpleDateFormat("dd-MM-yyyy")));
		columns.add(new EnumPropertyColumn<Complicatie, String, ComplicatieSoort>(Model.of("Soort"), "soort", "soort"));
		columns.add(new EnumPropertyColumn<Complicatie, String, ComplicatieErnst>(Model.of("Ernst"), "ernst", "ernst"));
		columns.add(new EnumPropertyColumn<Complicatie, String, ComplicatieSoort>(Model.of("Moment"), "moment", "moment"));
		columns.add(new DateTimePropertyColumn<Complicatie, String>(Model.of("Datum onderzoek"), "mdlverslag.datumOnderzoek", "mdlverslag", new SimpleDateFormat("dd-MM-yyyy")));

		columns.add(new PropertyColumn<Complicatie, String>(Model.of("Uitvoerende organisatie"), "mdlverslag", "mdlverslag.uitvoerderOrganisatie.naam"));
		complicatieModel = ModelUtil.cModel(new Complicatie());
		Complicatie complicatie = complicatieModel.getObject();
		complicatie.setClient(ModelUtil.nullSafeGet(model));

		final WebMarkupContainer tooltipContainter = new WebMarkupContainer("tooltipContainter");
		add(tooltipContainter);
		tooltipContainter.setOutputMarkupId(true);
		final RepeatingView tooltips = new RepeatingView("tooltip");
		tooltipContainter.add(tooltips);

		ScreenitDataTable<Complicatie, String> table = new ScreenitDataTable<Complicatie, String>("complicaties", columns, new ComplicatieDataProvider(complicatieModel), 10,
			new Model<String>("complicaties"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target, IModel<Complicatie> model)
			{
				model = ModelUtil.cModel(model.getObject());
				setResponsePage(new ClientComplicatieEditPage(ClientComplicatieOverzichtPanel.this.getModel(), model));
			}

			@Override
			protected Item<Complicatie> newRowItem(String id, int index, IModel<Complicatie> model)
			{
				Item<Complicatie> item = super.newRowItem(id, index, model);
				String tooltipId = "tooltip-" + getRowModel().getObject().getId();
				item.add(new AttributeAppender("data-tooltip", Model.of(tooltipId)));
				if (!addedTooltips.contains(tooltipId))
				{
					tooltips.add(new ComplicatieToolTip(tooltips.newChildId(), getRowModel()));
					addedTooltips.add(tooltipId);
				}
				return item;
			}

		};

		add(table);
	}

	@Override
	protected void detachModel()
	{
		super.detachModel();
		ModelUtil.nullSafeDetach(complicatieModel);
	}

	private class ComplicatieToolTip extends Fragment
	{

		private static final long serialVersionUID = 1L;

		public ComplicatieToolTip(String id, IModel<Complicatie> complicatieModel)
		{
			super(id, "tooltipFragment", ClientComplicatieOverzichtPanel.this, new CompoundPropertyModel<>(complicatieModel));
			add(new AttributeAppender("class", Model.of(" tooltip-" + complicatieModel.getObject().getId())));
			add(new Label("instellingGebruiker.medewerker.naamVolledig"));
		}
	}
}
