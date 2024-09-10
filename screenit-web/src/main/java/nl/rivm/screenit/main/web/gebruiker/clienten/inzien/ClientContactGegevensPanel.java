package nl.rivm.screenit.main.web.gebruiker.clienten.inzien;

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
import nl.rivm.screenit.main.web.component.validator.ScreenitTelefoonnummerValidator;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ClientService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import nl.rivm.screenit.main.web.component.validator.EmailAddressValidator;
import org.apache.wicket.validation.validator.StringValidator;

public class ClientContactGegevensPanel extends GenericPanel<Client>
{
	@SpringBean
	private ClientService clientService;

	private final boolean magWijzigen;

	public ClientContactGegevensPanel(String id, IModel<Client> model)
	{
		super(id, model);
		magWijzigen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_CONTACTGEGEVENS_REGISTREREN, Actie.AANPASSEN) && clientService.isClientActief(model.getObject());
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		var form = new Form<>("contactgegevensForm", getModel());
		var contactgegevens = new WebMarkupContainer("contactgegevens");

		contactgegevens.add(maakInvoerVeld("persoon.telefoonnummer1", GbaPersoon.MAX_PHONE_LENGTH).add(ScreenitTelefoonnummerValidator.mobielNederlandsNummer()));
		contactgegevens.add(maakInvoerVeld("persoon.telefoonnummer2", GbaPersoon.MAX_PHONE_LENGTH).add(ScreenitTelefoonnummerValidator.alle()));
		contactgegevens.add(maakEmailInvoer());

		contactgegevens.add(new IndicatingAjaxSubmitLink("opslaan")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				clientService.saveContactGegevens(getModelObject(), ScreenitSession.get().getLoggedInInstellingGebruiker());
				info(getString("contactgegevens.opgeslagen"));
			}
		}.setVisible(magWijzigen));

		form.add(contactgegevens);
		add(form);
	}

	private FormComponent<String> maakEmailInvoer()
	{
		return maakInvoerVeld("persoon.emailadres", GbaPersoon.MAX_EMAIL_LENGTH)
			.add(EmailAddressValidator.getInstance())
			.setLabel(Model.of(getString("emailadres.label")));
	}

	private TextField<String> maakInvoerVeld(String id, int maxLength)
	{
		var textField = new TextField<String>(id)
		{
			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(magWijzigen || StringUtils.isNotBlank(getModelObject()));
			}
		};
		textField.setEnabled(magWijzigen);
		textField.add(StringValidator.maximumLength(maxLength));
		return textField;
	}
}
