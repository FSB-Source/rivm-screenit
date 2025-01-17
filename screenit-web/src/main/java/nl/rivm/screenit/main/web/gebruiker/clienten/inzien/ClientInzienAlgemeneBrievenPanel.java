package nl.rivm.screenit.main.web.gebruiker.clienten.inzien;

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

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.service.DossierService;
import nl.rivm.screenit.main.util.BriefOmschrijvingUtil;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.modal.IDialogCloseCallback;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.GebeurtenisComparator;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.GebeurtenisPopupBasePanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.BezwaarService;
import nl.topicuszorg.wicket.model.DetachableListModel;
import nl.topicuszorg.wicket.model.SortingListModel;

import org.apache.wicket.ajax.AjaxEventBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.list.PropertyListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class ClientInzienAlgemeneBrievenPanel extends GenericPanel<Client>
{
	@SpringBean
	private DossierService dossierService;

	@SpringBean
	private BezwaarService bezwaarService;

	private final BootstrapDialog dialog;

	private WebMarkupContainer gebeurtenissenContainer;

	private ListView<ScreeningRondeGebeurtenis> listView;

	ClientInzienAlgemeneBrievenPanel(String id, IModel<Client> model, BootstrapDialog dialog)
	{
		super(id, model);
		this.dialog = dialog;

		gebeurtenissenContainer = new WebMarkupContainer("gebeurtenissenContainer");
		gebeurtenissenContainer.setOutputMarkupId(true);

		listView = getGebeurtenissenListView();
		gebeurtenissenContainer.add(listView);
		add(gebeurtenissenContainer);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
	}

	private ListView<ScreeningRondeGebeurtenis> getGebeurtenissenListView()
	{
		var algemeneBriefGebeurtenissen = dossierService.getAlgemeneBriefGebeurtenissen(teTonenBrieven());
		var dossierModel = new DetachableListModel<>(algemeneBriefGebeurtenissen);
		var gebeurtenissenListView = new PropertyListView<>("gebeurtenissen",
			new SortingListModel<>(dossierModel, new GebeurtenisComparator()))
		{
			@Override
			protected void populateItem(ListItem<ScreeningRondeGebeurtenis> item)
			{
				var gebeurtenis = item.getModelObject();
				item.add(DateLabel.forDatePattern("datum", Model.of(gebeurtenis.getDatum()), Constants.DEFAULT_DATE_TIME_SECONDS_FORMAT));
				item.add(new EnumLabel<>("gebeurtenis", gebeurtenis.getGebeurtenis()));
				addExtraOmschrijvingItem(item);
				item.add(new EnumLabel<>("bron", gebeurtenis.getBron()));
				item.add(new AjaxEventBehavior("click")
				{
					@Override
					protected void onEvent(AjaxRequestTarget target)
					{
						dialog.setCloseCallback((IDialogCloseCallback) target1 ->
						{
							listView = getGebeurtenissenListView();
							var nieuwGebCont = new WebMarkupContainer("gebeurtenissenContainer");
							nieuwGebCont.setOutputMarkupId(true);
							nieuwGebCont.add(listView);
							gebeurtenissenContainer.replaceWith(nieuwGebCont);
							gebeurtenissenContainer = nieuwGebCont;
							target1.add(gebeurtenissenContainer);
						});
						dialog.openWith(target, new GebeurtenisPopupBasePanel(IDialog.CONTENT_ID, item.getModel()));
					}
				});
			}
		};

		gebeurtenissenListView.setVisible(!algemeneBriefGebeurtenissen.isEmpty());
		gebeurtenissenListView.setOutputMarkupId(true);
		return gebeurtenissenListView;
	}

	private List<ClientBrief<?, ?, ?>> teTonenBrieven()
	{
		return Stream.concat(
				getModelObject().getAlgemeneBrieven().stream()
					.filter(b -> !List.of(BriefType.CLIENT_INZAGE_PERSOONSGEGEVENS_AANVRAAG, BriefType.CLIENT_INZAGE_PERSOONSGEGEVENS_HANDTEKENING).contains(b.getBriefType())),
				bezwaarService.getBezwaarBrievenVanClient(getModelObject()).stream()
					.filter(b -> BriefType.CLIENT_BEZWAAR_AANVRAAG_BRIEVEN.contains(b.getBriefType())))
			.collect(Collectors.toList());
	}

	@Override
	public boolean isVisible()
	{
		return !teTonenBrieven().isEmpty();
	}

	private void addExtraOmschrijvingItem(final ListItem<ScreeningRondeGebeurtenis> item)
	{
		item.add(new Label("extraOmschrijving", (IModel<String>) () ->
		{
			ScreeningRondeGebeurtenis screeningRondeGebeurtenis = item.getModelObject();
			String[] extraOmschrijvingen = screeningRondeGebeurtenis.getExtraOmschrijving();
			return BriefOmschrijvingUtil.verwerkExtraOmschrijvingen(extraOmschrijvingen, ClientInzienAlgemeneBrievenPanel.this::getString);
		})
		{
			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(item.getModelObject().getExtraOmschrijving() != null);
			}

		});
	}
}
