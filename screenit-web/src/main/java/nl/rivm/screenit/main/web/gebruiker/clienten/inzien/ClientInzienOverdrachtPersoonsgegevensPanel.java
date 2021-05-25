package nl.rivm.screenit.main.web.gebruiker.clienten.inzien;

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

import nl.rivm.screenit.main.model.DossierGebeurtenisType;
import nl.rivm.screenit.main.model.OverdrachtPersoonsgegevensDossierGebeurtenis;
import nl.rivm.screenit.main.service.DossierService;
import nl.rivm.screenit.main.util.BriefOmschrijvingUtil;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.clienten.inzien.popup.overdrachtpersoonsgegevens.OverdrachtGegevensAanvraagPopupPanel;
import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.algemeen.OverdrachtPersoonsgegevens;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxEventBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ClientInzienOverdrachtPersoonsgegevensPanel extends GenericPanel<Client>
{
	private static final long serialVersionUID = 1L;

	private BootstrapDialog dialog;

	@SpringBean
	private DossierService dossierService;

	private WebMarkupContainer gebeurtenissenContainer;

	ClientInzienOverdrachtPersoonsgegevensPanel(String id, IModel<Client> model, BootstrapDialog dialog)
	{
		super(id, model);

		this.dialog = dialog;
		addOrReplaceOverdrachtContainer();
	}

	private WebMarkupContainer addOrReplaceOverdrachtContainer()
	{
		WebMarkupContainer container = new WebMarkupContainer("gebeurtenissenContainer");
		container.setOutputMarkupPlaceholderTag(true);

		Client client = getModelObject();
		ArrayList<OverdrachtPersoonsgegevensDossierGebeurtenis> gebeurtenissen = new ArrayList<>();

		for (OverdrachtPersoonsgegevens overdracht : client.getOverdrachtPersoonsgegevensLijst())
		{
			OverdrachtPersoonsgegevensDossierGebeurtenis gebeurtenis = new OverdrachtPersoonsgegevensDossierGebeurtenis(overdrachtOmschrijving(overdracht),
				overdracht.getStatusDatum());
			gebeurtenis.setDossierGebeurtenisType(DossierGebeurtenisType.OVERDRACHT_PERSOONSGEGEVENS);
			gebeurtenis.setBron(dossierService.bepaalGebeurtenisBron(overdracht));
			gebeurtenis.setOverdrachtPersoonsgegevensModel(ModelUtil.sModel(overdracht));
			gebeurtenissen.add(gebeurtenis);
		}

		container.add(new ListView<OverdrachtPersoonsgegevensDossierGebeurtenis>("gebeurtenissen", gebeurtenissen)
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<OverdrachtPersoonsgegevensDossierGebeurtenis> item)
			{
				OverdrachtPersoonsgegevensDossierGebeurtenis gebeurtenis = item.getModelObject();
				WebMarkupContainer gebeurtenisContainer = new WebMarkupContainer("gebeurtenis");
				gebeurtenisContainer.add(new Label("omschrijving", new PropertyModel<String>(gebeurtenis, "omschrijving")));
				gebeurtenisContainer.add(DateLabel.forDatePattern("tijd", Model.of(gebeurtenis.getTijd()), "dd-MM-yyyy HH:mm:ss"));
				gebeurtenisContainer.add(new EnumLabel<>("bron", Model.of(gebeurtenis.getBron())));

				gebeurtenisContainer.add(new AjaxEventBehavior("click")
				{
					private static final long serialVersionUID = 1L;

					@Override
					protected void onEvent(AjaxRequestTarget target)
					{
						dialog.openWith(target, new OverdrachtGegevensAanvraagPopupPanel(IDialog.CONTENT_ID, gebeurtenis.getOverdrachtPersoonsgegevensModel())
						{
							@Override
							public void close(AjaxRequestTarget target)
							{
								target.add(addOrReplaceOverdrachtContainer());
								dialog.close(target);
							}
						});
					}

				});
				item.add(gebeurtenisContainer);
			}
		});
		if (gebeurtenissenContainer != null)
		{
			gebeurtenissenContainer.replaceWith(container);
		}
		gebeurtenissenContainer = container;
		add(gebeurtenissenContainer);
		return gebeurtenissenContainer;
	}

	private String overdrachtOmschrijving(OverdrachtPersoonsgegevens overdracht)
	{
		switch (overdracht.getStatus())
		{
		case BRIEF:
			StringBuilder omschrijving = new StringBuilder(getString("overdracht.aanvraag"));
			Brief brief = overdracht.getGeenHandtekeningBrief() != null ? overdracht.getGeenHandtekeningBrief() : overdracht.getVerstuurdeAanvraagbrief();
			BriefOmschrijvingUtil.addExtraOmschrijving(omschrijving, brief, this::getString);
			return omschrijving.toString();
		case BRIEF_ONTVANGEN:
			return getString("overdracht.getekende.aanvraag");
		case VERWERKT:
			return getString("overdracht.afgerond");
		default:
			throw new IllegalStateException("Foute status " + overdracht.getStatus());
		}
	}

	@Override
	public boolean isVisible()
	{
		return !getModelObject().getOverdrachtPersoonsgegevensLijst().isEmpty();
	}
}
