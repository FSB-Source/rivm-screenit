package nl.rivm.screenit.main.web.client.dashboard.colon;

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
import java.util.List;
import java.util.Optional;

import nl.rivm.screenit.model.Afspraak;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.service.colon.AfspraakService;
import nl.rivm.screenit.util.AdresUtil;
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

public class AfsprakenPanel extends GenericPanel<Client>
{
	@SpringBean
	private AfspraakService afspraakService;

	@SpringBean
	private HibernateService hibernateService;

	private WebMarkupContainer afsprakenContainer;

	public AfsprakenPanel(String id, IModel<Client> model)
	{
		super(id, model);

		afsprakenContainer = new WebMarkupContainer("afsprakenContainer");
		afsprakenContainer.setOutputMarkupId(true);
		add(afsprakenContainer);

		List<Afspraak> afspraken = new ArrayList<>();
		for (Afspraak afspraak : model.getObject().getAfspraken())
		{
			if (afspraakService.magWijzigenAfzeggen(afspraak))
			{
				afspraken.add(afspraak);
			}
		}

		afsprakenContainer.setVisible(afspraken.size() > 0);
		afsprakenContainer.add(new ListView<Afspraak>("afspraken", ModelUtil.listRModel(afspraken))
		{
			@Override
			protected void populateItem(ListItem<Afspraak> item)
			{
				Afspraak afspraak = item.getModelObject();
				item.add(DateLabel.forDatePattern("dag", Model.of(afspraak.getStartTime()), "dd"));
				item.add(DateLabel.forDatePattern("maand", Model.of(afspraak.getStartTime()), "MMM"));
				item.add(DateLabel.forDatePattern("tijd", Model.of(afspraak.getStartTime()), "HH:mm"));

				ColoscopieCentrum centrum = afspraak.getLocation().getColoscopieCentrum();
				item.add(new Label("ccNaam", centrum.getNaam()));
				item.add(new Label("ccAdres", AdresUtil.getVolledigeAdresString(centrum.getAdressen().get(0))));

				item.add(new AjaxFallbackLink<Afspraak>("afzeggen", item.getModel())
				{
					private static final long serialVersionUID = 1L;

					@Override
					public void onClick(Optional<AjaxRequestTarget> target)
					{
						setResponsePage(new ClientAfspraakAfzeggenPage(getNieuwAfspraakModel(getModel())));
					}
				});

				item.add(new AjaxFallbackLink<Afspraak>("verplaatsen", item.getModel())
				{
					private static final long serialVersionUID = 1L;

					@Override
					public void onClick(Optional<AjaxRequestTarget> target)
					{
						setResponsePage(new ClientAfspraakVerplaatsenPage(getNieuwAfspraakModel(getModel())));
					}

				});
			}

			private IModel<ColonIntakeAfspraak> getNieuwAfspraakModel(IModel<Afspraak> model)
			{
				ColonIntakeAfspraak intakeAfspraak = hibernateService.load(ColonIntakeAfspraak.class, model.getObject().getId());
				intakeAfspraak = intakeAfspraak.getColonScreeningRonde().getLaatsteAfspraak();
				return ModelUtil.ccModel(intakeAfspraak);
			}
		});
	}
}
