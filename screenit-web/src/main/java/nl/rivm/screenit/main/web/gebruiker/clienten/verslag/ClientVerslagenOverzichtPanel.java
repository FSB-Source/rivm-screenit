package nl.rivm.screenit.main.web.gebruiker.clienten.verslag;

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

import java.util.ArrayList;
import java.util.List;

import nl.dries.wicket.hibernate.dozer.DozerModel;
import nl.rivm.screenit.Constants;
import nl.rivm.screenit.service.RondeNummerService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.cervix.CervixCytologieVerslag;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.verslag.followup.MammaFollowUpVerslagContent;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.head.CssHeaderItem;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.head.PriorityHeaderItem;
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

public abstract class ClientVerslagenOverzichtPanel<V extends Verslag<?, ?>> extends GenericPanel<Client>
{
	private static final long serialVersionUID = 1L;

	private List<String> addedTooltips = new ArrayList<>();

	@SpringBean
	private RondeNummerService rondeNummerService;

	private ClientVerslagenDataProvider<V> dataProvider;

	public ClientVerslagenOverzichtPanel(String id, IModel<Client> model)
	{
		super(id, model);

		List<IColumn<V, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<V, String>(Model.of("Ronde"), "ronde")
		{
			private static final long serialVersionUID = 1L;

			@Override
			public IModel<Object> getDataModel(IModel<V> rowModel)
			{
				return new Model(rondeNummerService.geefRondeNummer(rowModel.getObject().getScreeningRonde()));
			}
		});
		columns.add(new EnumPropertyColumn<V, String, VerslagType>(Model.of("Type verslag"), "type"));
		columns.add(new DateTimePropertyColumn<V, String>(Model.of("Datum onderzoek"), "datumOnderzoek", "datumOnderzoek"));
		columns.add(new DateTimePropertyColumn<V, String>(Model.of("Datum/tijd verslag"), "datumVerwerkt", "datumVerwerkt"));
		columns.add(new EnumPropertyColumn<V, String, VerslagStatus>(Model.of("Status"), "status", "status")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public IModel<?> getDataModel(IModel<V> rowModel)
			{
				IModel<?> dataModel = super.getDataModel(rowModel);
				if (dataModel instanceof Model && rowModel.getObject().getType() == VerslagType.CERVIX_CYTOLOGIE
					&& ((CervixCytologieVerslag) HibernateHelper.deproxy(rowModel.getObject())).getUitstrijkje().getVerwijderdDatum() != null)
				{
					dataModel = new Model(dataModel.getObject() + " (Verwijderd)");
				}
				return dataModel;
			}

		});

		final WebMarkupContainer tooltipContainter = new WebMarkupContainer("tooltipContainter");
		add(tooltipContainter);
		tooltipContainter.setOutputMarkupId(true);
		final RepeatingView tooltips = new RepeatingView("tooltip");
		tooltipContainter.add(tooltips);

		IModel<? extends V> verslagenFilterModel = getVerslagFilter();

		dataProvider = new ClientVerslagenDataProvider<V>(verslagenFilterModel);
		add(new ScreenitDataTable<V, String>("tabel", columns, dataProvider, 10,
			new Model<>("verslag(en)"))
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target, IModel<V> model)
			{

				IModel<Client> clientModel = ClientVerslagenOverzichtPanel.this.getModel();
				boolean inzien = false;
				switch (model.getObject().getType())
				{
					case MDL:
						inzien = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_UITSLAGCOLOSCOPIEONTVANGEN, Actie.INZIEN, clientModel.getObject());
						break;
					case PA_LAB:
						inzien = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_UITSLAGPATHOLOGIEONTVANGEN, Actie.INZIEN, clientModel.getObject());
						break;
					case CERVIX_CYTOLOGIE:
						inzien = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CERVIX_CYTOLOGIE_VERSLAG, Actie.INZIEN, clientModel.getObject());
						break;
					case MAMMA_PA_FOLLOW_UP:
						inzien = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_MAMMA_FOLLOW_UP_VERSLAG, Actie.INZIEN, clientModel.getObject());
						break;
				}
				if (inzien)
				{
					setResponsePage(new ClientVerslagPage(new DozerModel<>(model.getObject())));
				}

			}

			@Override
			protected Item<V> newRowItem(final String id, final int index, final IModel<V> model)
			{
				Item<V> item = super.newRowItem(id, index, model);
				String tooltipId = "tooltip-" + getRowModel().getObject().getId();
				item.add(new AttributeAppender("data-tooltip", Model.of(tooltipId)));
				item.add(new AttributeAppender("class", Model.of(" status-" + model.getObject().getStatus().name().toLowerCase())));
				if (!addedTooltips.contains(tooltipId))
				{
					tooltips.add(new VerslagTooltip(tooltips.newChildId(), getRowModel()));
					addedTooltips.add(tooltipId);
				}
				return item;
			}

			@Override
			protected void updateContainer(AjaxRequestTarget target)
			{
				super.updateContainer(target);
				target.add(tooltipContainter);
				target.appendJavaScript("initTooltip();");
			}
		});
	}

	@Override
	protected void onConfigure()
	{

		super.onConfigure();
		setVisible(dataProvider != null && dataProvider.size() > 0 && magOverzichtZien());
	}

	private class VerslagTooltip extends Fragment
	{
		private static final long serialVersionUID = 1L;

		public VerslagTooltip(String id, IModel<V> model)
		{
			super(id, "tooltipFragment", ClientVerslagenOverzichtPanel.this, new CompoundPropertyModel<>(model));
			V verslag = model.getObject();
			add(new AttributeAppender("class", Model.of(" tooltip-" + verslag.getId())));
			add(new Label("uitvoerderMedewerker.naamVolledig").setVisible(verslag.getUitvoerderMedewerker() != null));
			add(new Label("uitvoerderOrganisatie.naam"));
			add(new Label("invoerder.medewerker.naamVolledig").setVisible(verslag.getInvoerder() != null));
			add(new Label("invoerder.organisatie.naam"));
			if (verslag.getType() == VerslagType.MAMMA_PA_FOLLOW_UP)
			{
				add(new Label("bron",
					Constants.BK_TNUMMER_ELEKTRONISCH.equals(((MammaFollowUpVerslagContent) verslag.getVerslagContent()).getPathologieMedischeObservatie().getTnummerLaboratorium())
						? "Elektronisch"
						: "Handmatig"));
			}
			else
			{
				add(new Label("bron", Model.of(verslag.getOntvangenBericht() != null ? "Elektronisch" : "Handmatig")));
			}
		}
	}

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);
		response.render(new PriorityHeaderItem(CssHeaderItem.forUrl("assets/js/libs/qtip/jquery.qtip.min.css")));
		response.render(new PriorityHeaderItem(JavaScriptHeaderItem.forUrl("assets/js/libs/qtip/jquery.qtip.min.js")));
		response.render(new OnDomReadyHeaderItem("initTooltip()"));
	}

	protected abstract IModel<? extends V> getVerslagFilter();

	protected abstract boolean magOverzichtZien();
}
