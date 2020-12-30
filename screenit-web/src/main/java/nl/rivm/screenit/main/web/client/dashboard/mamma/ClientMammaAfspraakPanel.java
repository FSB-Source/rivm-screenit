package nl.rivm.screenit.main.web.client.dashboard.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxFallbackLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class ClientMammaAfspraakPanel extends GenericPanel<Client>
{
	@SpringBean
	private MammaBaseAfspraakService afspraakService;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private HibernateService hibernateService;

	private WebMarkupContainer afsprakenContainer;

	public ClientMammaAfspraakPanel(String id, IModel<Client> model)
	{
		super(id, model);

		afsprakenContainer = new WebMarkupContainer("afsprakenContainer");
		afsprakenContainer.setOutputMarkupId(true);
		add(afsprakenContainer);

		MammaDossier dossier = model.getObject().getMammaDossier();
		MammaAfspraak afspraak = MammaScreeningRondeUtil.getLaatsteAfspraak(dossier.getLaatsteScreeningRonde());

		afsprakenContainer.setVisible(afspraak != null && afspraak.getStatus() == MammaAfspraakStatus.GEPLAND && afspraak.getVanaf().compareTo(dateSupplier.getDate()) >= 0);
		afsprakenContainer.add(new ListView<MammaAfspraak>("afspraken", ModelUtil.listRModel(Arrays.asList(afspraak)))
		{
			@Override
			protected void populateItem(ListItem<MammaAfspraak> item)
			{
				MammaAfspraak afspraak = item.getModelObject();
				IModel<Date> vanafModel = Model.of(afspraak.getVanaf());
				item.add(DateLabel.forDatePattern("dag", vanafModel, "dd"));
				item.add(DateLabel.forDatePattern("maand", vanafModel, "MMM"));
				item.add(DateLabel.forDatePattern("tijd", vanafModel, "HH:mm"));

				MammaStandplaats standplaats = afspraak.getStandplaatsPeriode().getStandplaatsRonde().getStandplaats();
				item.add(new Label("naam", standplaats.getNaam()));
				item.add(new Label("adres",
					AdresUtil.getAdresVoorStandplaatsLocatie(afspraakService.getMammaStandplaatsLocatieAfspraak(afspraak))));

				item.add(new AjaxFallbackLink<MammaAfspraak>("afzeggen")
				{
					private static final long serialVersionUID = 1L;

					@Override
					public void onClick(Optional<AjaxRequestTarget> target)
					{
						List<Object> extraPanelParams = new ArrayList<>();
						extraPanelParams.add(AanvraagBriefStatus.BRIEF);
						extraPanelParams.add(ClientContactActieType.MAMMA_AFMELDEN);
						setResponsePage(
							new ClientMammaOverigeDashboardActiePage(ClientMammaAfspraakPanel.this.getModel(), ClientMammaAfmeldenPanel.class,
								ClientContactActieType.MAMMA_AFMELDEN, extraPanelParams));
					}
				});

				item.add(new AjaxFallbackLink<MammaAfspraak>("verplaatsen", item.getModel())
				{
					private static final long serialVersionUID = 1L;

					@Override
					public void onClick(Optional<AjaxRequestTarget> target)
					{
						setResponsePage(new ClientMammaAfspraakVerzettenPage(ModelUtil.ccModel(getModelObject().getUitnodiging().getLaatsteAfspraak())));
					}
				});
			}

		});
	}
}
