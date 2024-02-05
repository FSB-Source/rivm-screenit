package nl.rivm.screenit.main.web.gebruiker.clienten.agenda;

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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.Afspraak;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.colon.AfspraakService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public abstract class ColonAfspraakPanel extends GenericPanel<Client>
{
	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private AfspraakService afspraakService;

	public ColonAfspraakPanel(String id, IModel<Client> model)
	{
		super(id, model);

		hibernateService.reload(model.getObject());
		var container = new WebMarkupContainer("headerWijzigen");
		add(container);

		ListView<Afspraak> afspraken = new ListView<Afspraak>("afspraken", new PropertyModel<>(model, "afspraken"))
		{
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
				Afspraak afspraak = item.getModelObject();
				boolean heeftOnafgerondeVerwijzingOmMedischeRedenen = afspraakService.heeftOnafgerondeVerwijzingOmMedischeRedenen(afspraak);
				item.setVisible(afspraakService.magWijzigenAfzeggen(afspraak) || heeftOnafgerondeVerwijzingOmMedischeRedenen);
				AjaxLink<Afspraak> afzeggen = new IndicatingAjaxLink<Afspraak>("afzeggen", item.getModel())
				{
					@Override
					public void onClick(AjaxRequestTarget target)
					{
						afspraakAfzeggen(target, getModelObject());
					}

				};
				afzeggen.setVisible(!heeftOnafgerondeVerwijzingOmMedischeRedenen);

				item.add(afzeggen);
				AjaxLink<Afspraak> tijdstipWijzigen = new IndicatingAjaxLink<Afspraak>("tijdstipWijzigen", item.getModel())
				{

					@Override
					public void onClick(AjaxRequestTarget target)
					{
						afspraakWijzigen(target, getModelObject(), false);
					}

				};

				tijdstipWijzigen.setVisible(!heeftOnafgerondeVerwijzingOmMedischeRedenen);

				item.add(tijdstipWijzigen);
				AjaxLink<Afspraak> locatieWijzigen = new IndicatingAjaxLink<Afspraak>("locatieWijzigen", item.getModel())
				{

					@Override
					public void onClick(AjaxRequestTarget target)
					{
						afspraakWijzigen(target, getModelObject(), true);
					}

				};
				item.add(locatieWijzigen);
				zetLocatieEnTijdstipInvisible(tijdstipWijzigen, locatieWijzigen, container, model);
			}
		};
		add(afspraken);
		setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_INTAKEAFSPRAAKGEMAAKT, Actie.INZIEN));
	}

	public abstract void afspraakWijzigen(AjaxRequestTarget target, Afspraak afspraak, boolean locatieWijzigen);

	public abstract void afspraakAfzeggen(AjaxRequestTarget target, Afspraak afspraak);

	private void zetLocatieEnTijdstipInvisible(AjaxLink<Afspraak> tijdstipWijzigen, AjaxLink<Afspraak> locatieWijzigen, WebMarkupContainer containerHeaderWijzigen,
		IModel<Client> model)
	{
		var vertrokkenUitNederland = model.getObject().getPersoon().getDatumVertrokkenUitNederland();
		if (vertrokkenUitNederland != null)
		{
			tijdstipWijzigen.setVisible(false);
			locatieWijzigen.setVisible(false);
			containerHeaderWijzigen.setVisible(false);
		}
		else
		{
			tijdstipWijzigen.setVisible(true);
			locatieWijzigen.setVisible(true);
			containerHeaderWijzigen.setVisible(true);
		}
	}
}
