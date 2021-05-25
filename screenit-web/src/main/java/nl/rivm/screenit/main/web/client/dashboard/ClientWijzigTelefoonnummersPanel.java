
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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.client.base.ClientHoofdMenuitem;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.TijdelijkAdres;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.wicket.input.validator.TelefoonnummerValidator;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ClientWijzigTelefoonnummersPanel extends GenericPanel<Client>
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private ClientService clientService;

	@SpringBean
	private LogService logService;

	public ClientWijzigTelefoonnummersPanel(String id, IModel<Client> model, ClientHoofdMenuitem menu)
	{
		super(id, new CompoundPropertyModel<>(model));

		Client client = model.getObject();
		if (client.getPersoon().getTijdelijkAdres() == null)
		{
			client.getPersoon().setTijdelijkAdres(new TijdelijkAdres());
		}
		Form<Client> form = new ScreenitForm<Client>("form", getModel());
		add(form);

		form.add(new TextField<String>("persoon.telefoonnummer1").add(TelefoonnummerValidator.alle()).setOutputMarkupId(true));
		form.add(new TextField<String>("persoon.telefoonnummer2").add(TelefoonnummerValidator.alle()).setOutputMarkupId(true));

		form.add(new Link<Void>("annuleren")
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick()
			{
				setResponsePage(menu.getTargetClass());
			}

		});

		form.add(new AjaxSubmitLink("opslaan")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				clientService.saveOrUpdateClient(getModelObject());
				ScreenitSession.get().success(getString("success.telefoonnummers"));
				logService.logGebeurtenis(LogGebeurtenis.CLIENT_CONTACT_GEGEVENS_GEWIJZIGD, getModelObject());
				setResponsePage(menu.getTargetClass());
			}

		});
	}

}
