
package nl.rivm.screenit.main.web.gebruiker.clienten.agenda;

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

import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.Afspraak;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.colon.AfspraakService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class ColonAfspraakPanel extends GenericPanel<Client>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private AfspraakService afspraakService;

	public ColonAfspraakPanel(String id, IModel<Client> model)
	{
		super(id, model);

		hibernateService.reload(model.getObject());

		ListView<Afspraak> afspraken = new ListView<Afspraak>("afspraken", new PropertyModel<List<Afspraak>>(model, "afspraken"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<Afspraak> item)
			{
				item.setDefaultModel(new CompoundPropertyModel<>(item.getModel()));
				item.add(DateLabel.forDatePattern("startTime", "EEEE dd-MM-yyyy HH:mm"));
				item.add(new Label("location.coloscopieCentrum.naam"));
				item.add(new Label("location.coloscopieCentrum.adressen[0].straat"));
				item.add(new Label("location.coloscopieCentrum.adressen[0].huisnummer"));
				item.add(new Label("location.coloscopieCentrum.adressen[0].postcode"));
				item.add(new Label("location.coloscopieCentrum.adressen[0].plaats"));
				item.add(new Label("location.coloscopieCentrum.locatieBeschrijving"));
				AjaxLink<Afspraak> afzeggen = new IndicatingAjaxLink<Afspraak>("afzeggen", item.getModel())
				{

					private static final long serialVersionUID = 1L;

					@Override
					public void onClick(AjaxRequestTarget target)
					{
						afspraakAfzeggen(target, getModelObject());
					}

				};
				Afspraak afspraak = item.getModelObject();
				item.setVisible(afspraakService.magWijzigenAfzeggen(afspraak));
				item.add(afzeggen);
				AjaxLink<Afspraak> tijdstipWijzigen = new IndicatingAjaxLink<Afspraak>("tijdstipWijzigen", item.getModel())
				{

					private static final long serialVersionUID = 1L;

					@Override
					public void onClick(AjaxRequestTarget target)
					{
						afspraakWijzigen(target, getModelObject(), false);
					}

				};
				item.add(tijdstipWijzigen);
				AjaxLink<Afspraak> locatieWijzigen = new IndicatingAjaxLink<Afspraak>("locatieWijzigen", item.getModel())
				{

					private static final long serialVersionUID = 1L;

					@Override
					public void onClick(AjaxRequestTarget target)
					{
						afspraakWijzigen(target, getModelObject(), true);
					}

				};
				item.add(locatieWijzigen);
			}
		};
		add(afspraken);
		setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_INTAKEAFSPRAAKGEMAAKT, Actie.INZIEN));
	}

	public abstract void afspraakWijzigen(AjaxRequestTarget target, Afspraak afspraak, boolean locatieWijzigen);

	public abstract void afspraakAfzeggen(AjaxRequestTarget target, Afspraak afspraak);
}
