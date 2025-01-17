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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import nl.rivm.screenit.comparator.BezwaarComparator;
import nl.rivm.screenit.main.model.BezwaarDossierGebeurtenis;
import nl.rivm.screenit.main.model.DossierGebeurtenisType;
import nl.rivm.screenit.main.service.DossierService;
import nl.rivm.screenit.main.web.component.bezwaar.tekst.BezwaarTekstPanel;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.clienten.inzien.popup.bezwaar.BezwaarInzienPopupPanel;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.util.BezwaarUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxEventBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class ClientInzienBezwaarPanel extends GenericPanel<Client>
{
	@SpringBean
	private DossierService dossierService;

	private final BootstrapDialog dialog;

	private WebMarkupContainer meldingenContainer;

	private WebMarkupContainer actueleBezwarenContainer;

	public ClientInzienBezwaarPanel(String id, IModel<Client> model, BootstrapDialog dialog)
	{
		super(id, model);
		this.dialog = dialog;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		addOrReplaceMeldingContainer();
		addOrReplaceActueleBezwarenContainer();
	}

	private void addOrReplaceActueleBezwarenContainer()
	{
		WebMarkupContainer container = new WebMarkupContainer("actueleBezwarenContainer");
		container.setOutputMarkupPlaceholderTag(true);
		container.setVisible(false);

		List<BezwaarMoment> momenten = getModelObject().getBezwaarMomenten();
		Collections.sort(momenten, new BezwaarComparator());

		BezwaarMoment laatsteAfgerondeBezwaarMoment = getLaatstAfgerondeBezwaarMoment(momenten);
		Component bezwaarTekstPanel = new EmptyPanel("bezwaarTekstPanel");
		if (laatsteAfgerondeBezwaarMoment != null && !BezwaarUtil.isVerwijderDossierHetEnigeBezwaar(laatsteAfgerondeBezwaarMoment)
			&& !laatsteAfgerondeBezwaarMoment.getBezwaren().isEmpty())
		{
			bezwaarTekstPanel = new BezwaarTekstPanel("bezwaarTekstPanel", ModelUtil.sModel(laatsteAfgerondeBezwaarMoment), false);
			container.setVisible(true);
		}
		container.add(bezwaarTekstPanel);

		if (actueleBezwarenContainer != null)
		{
			actueleBezwarenContainer.replaceWith(container);
		}
		actueleBezwarenContainer = container;
		add(actueleBezwarenContainer);
	}

	private void addOrReplaceMeldingContainer()
	{
		WebMarkupContainer container = new WebMarkupContainer("meldingenContainer");
		container.setOutputMarkupPlaceholderTag(true);

		Client client = getModelObject();
		ArrayList<BezwaarDossierGebeurtenis> listMeldingen = new ArrayList<>();

		for (BezwaarMoment bezwaar : client.getBezwaarMomenten())
		{
			BezwaarDossierGebeurtenis c = new BezwaarDossierGebeurtenis(bezwaarOmschrijving(bezwaar), bezwaar.getStatusDatum());
			c.setDossierGebeurtenisType(DossierGebeurtenisType.BEZWAAR);
			c.setBron(dossierService.bepaalGebeurtenisBron(bezwaar));
			c.setBezwaarModel(ModelUtil.sModel(bezwaar));
			listMeldingen.add(c);
		}

		container.add(new ListView<>("meldingen", listMeldingen)
		{
			@Override
			protected void populateItem(ListItem<BezwaarDossierGebeurtenis> item)
			{
				BezwaarDossierGebeurtenis clientMelding = item.getModelObject();
				WebMarkupContainer meldingContainer = new WebMarkupContainer("melding");
				meldingContainer.add(new Label("omschrijving", new PropertyModel<String>(clientMelding, "omschrijving")));
				meldingContainer.add(DateLabel.forDatePattern("tijd", Model.of(clientMelding.getTijd()), "dd-MM-yyyy HH:mm:ss"));
				meldingContainer.add(new EnumLabel<>("bron", Model.of(clientMelding.getBron())));

				meldingContainer.add(new AjaxEventBehavior("click")
				{
					@Override
					protected void onEvent(AjaxRequestTarget target)
					{
						dialog.openWith(target, new BezwaarInzienPopupPanel(IDialog.CONTENT_ID, clientMelding.getBezwaarModel())
						{
							@Override
							protected void close(AjaxRequestTarget target)
							{
								dialog.close(target);
							}
						});
					}

				});
				item.add(meldingContainer);
			}
		});
		if (meldingenContainer != null)
		{
			meldingenContainer.replaceWith(container);
		}
		meldingenContainer = container;
		add(meldingenContainer);
	}

	private BezwaarMoment getLaatstAfgerondeBezwaarMoment(List<BezwaarMoment> momenten)
	{
		if (!momenten.isEmpty())
		{
			BezwaarMoment moment = momenten.get(0);
			if (AanvraagBriefStatus.VERWERKT.equals(moment.getStatus()))
			{
				return moment;
			}
			if (momenten.size() > 1)
			{
				return momenten.get(1);
			}
		}
		return null;
	}

	private String bezwaarOmschrijving(BezwaarMoment bezwaar)
	{
		StringBuilder omschrijving = new StringBuilder();
		omschrijving.append(getString("gebruik.gegevens.aangepast"));

		if (AanvraagBriefStatus.VERWERKT == bezwaar.getStatus() && bezwaar.getBezwaarBrief() != null)
		{
			omschrijving.append(" (").append(getString("label.formulier.getekendbezwaar")).append(")");
		}
		return omschrijving.toString();
	}

	@Override
	public boolean isVisible()
	{
		Client client = getModelObject();
		return !client.getBezwaarMomenten().isEmpty();
	}
}
