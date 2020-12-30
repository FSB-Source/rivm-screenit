
package nl.rivm.screenit.main.web.gebruiker.clienten.contact;

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

import nl.rivm.screenit.main.service.ClientContactService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.ClientContact;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.StringValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class ClientContactEditPanel extends GenericPanel<ClientContact>
{

	private static final Logger LOGGER = LoggerFactory.getLogger(ClientContactEditPanel.class);

	private static final long serialVersionUID = 1L;

	@SpringBean
	private ClientContactService clientContactService;

	public ClientContactEditPanel(String id, IModel<ClientContact> contactModel)
	{
		super(id, contactModel);

		Form<ClientContact> form = new Form<>("contactForm");
		add(form);
		form.add(new TextArea<String>("opmerking").add(StringValidator.maximumLength(2048)));
		form.add(new IndicatingAjaxButton("opslaan")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				success(getString("message.gegevensopgeslagen"));
				clientContactService.updateContact(ClientContactEditPanel.this.getModelObject(), ScreenitSession.get().getLoggedInInstellingGebruiker());
				ClientContactEditPanel.this.close(target);
			}
		});

		boolean magVerwijderen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_CONTACT, Actie.VERWIJDEREN, getModelObject().getClient())
			&& getModelObject().getActies().size() == 1 && getModelObject().getActies().get(0).getType() == ClientContactActieType.GEEN;
		form.add(new IndicatingAjaxButton("verwijderen")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				success(getString("contact.is.verwijderd"));
				clientContactService.verwijderContact(ClientContactEditPanel.this.getModelObject(), ScreenitSession.get().getLoggedInInstellingGebruiker());
				ClientContactEditPanel.this.close(target);
			}
		}.setVisible(magVerwijderen));
	}

	protected abstract void close(AjaxRequestTarget target);
}
