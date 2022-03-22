package nl.rivm.screenit.main.web.gebruiker.clienten.inzien;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.model.BriefDossierGebeurtenis;
import nl.rivm.screenit.main.model.DossierGebeurtenis;
import nl.rivm.screenit.main.model.DossierGebeurtenisType;
import nl.rivm.screenit.main.service.DossierService;
import nl.rivm.screenit.main.util.BriefOmschrijvingUtil;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.algemeen.AlgemeneBrief;
import nl.rivm.screenit.model.enums.BriefType;

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
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class ClientInzienAlgemeneBrievenPanel extends GenericPanel<Client>
{
	@SpringBean
	private DossierService dossierService;

	ClientInzienAlgemeneBrievenPanel(String id, IModel<Client> model)
	{
		super(id, model);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		WebMarkupContainer container = new WebMarkupContainer("gebeurtenissenContainer");
		container.setOutputMarkupPlaceholderTag(true);

		var gebeurtenissen = getGebeurtenissen();

		container.add(new ListView<>("gebeurtenissen", gebeurtenissen)
		{
			@Override
			protected void populateItem(ListItem<BriefDossierGebeurtenis> item)
			{
				var gebeurtenis = item.getModelObject();
				WebMarkupContainer gebeurtenisContainer = new WebMarkupContainer("gebeurtenis");
				gebeurtenisContainer.add(new Label("omschrijving", new PropertyModel<>(gebeurtenis, "omschrijving")));
				gebeurtenisContainer.add(DateLabel.forDatePattern("moment", Model.of(gebeurtenis.getTijd()), Constants.DEFAULT_DATE_TIME_SECONDS_FORMAT));
				gebeurtenisContainer.add(new EnumLabel<>("bron", Model.of(gebeurtenis.getBron())));
				item.add(gebeurtenisContainer);
			}
		});

		add(container);

		setVisible(!gebeurtenissen.isEmpty());
	}

	private List<BriefDossierGebeurtenis> getGebeurtenissen()
	{
		var gebeurtenissen = new ArrayList<BriefDossierGebeurtenis>();
		for (var brief : teTonenBrieven())
		{
			var gebeurtenis = new BriefDossierGebeurtenis(BriefOmschrijvingUtil.algemeneBriefOmschrijving(brief, this::getString),
				BriefOmschrijvingUtil.dossierGebeurtenisDatum(brief));
			gebeurtenis.setDossierGebeurtenisType(DossierGebeurtenisType.ALGEMENE_BRIEF);
			gebeurtenis.setBron(dossierService.bepaalGebeurtenisBron(brief));
			gebeurtenissen.add(gebeurtenis);
		}

		gebeurtenissen.sort(Comparator.comparing(DossierGebeurtenis::getTijd).reversed());
		return gebeurtenissen;
	}

	private List<AlgemeneBrief> teTonenBrieven()
	{
		return getModelObject().getAlgemeneBrieven().stream().filter(b -> b.getBriefType() == BriefType.CLIENT_SIGNALERING_GENDER).collect(Collectors.toList());
	}
}
