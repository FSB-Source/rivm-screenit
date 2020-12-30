package nl.rivm.screenit.main.web.client.dashboard;

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

import nl.rivm.screenit.main.web.client.base.ClientHoofdMenuitem;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.model.Client;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ClientOverdrachtPersoonsgegevensPanel extends GenericPanel<Client>
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private HibernateService hibernateService;

	public ClientOverdrachtPersoonsgegevensPanel(String id, IModel<String> title, IModel<Client> model, ClientHoofdMenuitem menu)
	{
		super(id, new CompoundPropertyModel<>(model));

		add(new Label("title", title));

		Link<Client> link = new Link<Client>("gegevensaanvragenLink", model)
		{
			@Override
			public void onClick()
			{
				setResponsePage(new ClientPersoonsgegevensAanvragenPage(ModelUtil.ccModel(ClientOverdrachtPersoonsgegevensPanel.this.getModelObject()), menu));
			}
		};
		add(link);

		link.add(new Label("gegevensaanvragenLinkTitle", new SimpleStringResourceModel("label.persoonsgegevensAanvragen")));
	}
}
