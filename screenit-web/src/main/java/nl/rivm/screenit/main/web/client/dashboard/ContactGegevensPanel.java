package nl.rivm.screenit.main.web.client.dashboard;

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

import nl.rivm.screenit.main.web.client.base.ClientHoofdMenuitem;
import nl.rivm.screenit.model.Client;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ContactGegevensPanel extends GenericPanel<Client>
{
	private ClientHoofdMenuitem menu;

	@SpringBean
	private HibernateService hibernateService;

	public ContactGegevensPanel(String id, IModel<String> title, IModel<Client> model, ClientHoofdMenuitem menu)
	{
		super(id, new CompoundPropertyModel<>(model));
		this.menu = menu;

		Client client = model.getObject();

		add(new Label("title", title));

		add(new Label("persoon.telefoonnummer1")); 
		add(new Label("persoon.telefoonnummer2")); 

		Link<Client> wijzigTelefoonnummers = createTelefoonnummersLink("wijzigTelefoonnummers");
		add(wijzigTelefoonnummers);
		Link<Client> opgevenTelefoonnummers = createTelefoonnummersLink("opgevenTelefoonnummers");
		add(opgevenTelefoonnummers);
		if (StringUtils.isBlank(client.getPersoon().getTelefoonnummer1()) && StringUtils.isBlank(client.getPersoon().getTelefoonnummer2()))
		{
			opgevenTelefoonnummers.setVisible(true);
			wijzigTelefoonnummers.setVisible(false);
		}
		else
		{
			opgevenTelefoonnummers.setVisible(false);
			wijzigTelefoonnummers.setVisible(true);
		}
	}

	private Link<Client> createTelefoonnummersLink(String id)
	{
		return new Link<Client>(id)
		{

			@Override
			public void onClick()
			{
				setResponsePage(new ClientWijzigTelefoonnummersPage(ModelUtil.ccModel(ContactGegevensPanel.this.getModelObject()), menu));
			}

		};
	}
}
