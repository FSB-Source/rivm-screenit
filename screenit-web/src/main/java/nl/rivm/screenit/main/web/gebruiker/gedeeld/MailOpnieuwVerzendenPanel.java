package nl.rivm.screenit.main.web.gebruiker.gedeeld;

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
import nl.rivm.screenit.model.DigitaalClientBericht;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaDigitaalClientBericht;
import nl.rivm.screenit.service.DigitaalClientBerichtService;
import nl.rivm.screenit.service.mamma.MammaDigitaalContactService;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import nl.rivm.screenit.main.web.component.validator.EmailAddressValidator;
import org.apache.wicket.validation.validator.StringValidator;

public class MailOpnieuwVerzendenPanel extends GenericPanel<DigitaalClientBericht<?>>
{
	@SpringBean
	private MammaDigitaalContactService mammaDigitaalContactService;

	@SpringBean
	private DigitaalClientBerichtService digitaalClientBerichtService;

	private IModel<String> nieuwEmailadresModel;

	public MailOpnieuwVerzendenPanel(String id, IModel<DigitaalClientBericht<?>> model)
	{
		super(id, model);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		add(maakMailOpnieuwVerzendenContainer());
	}

	private WebMarkupContainer maakMailOpnieuwVerzendenContainer()
	{
		var container = new WebMarkupContainer("herzendContainer");
		container.add(getEmailFormulier());
		var gebruikerMagMailsHerzenden = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_MAILS_OPNIEUW_VERZENDEN, Actie.AANPASSEN);
		container.setVisible(
			gebruikerMagMailsHerzenden && digitaalClientBerichtService.digitaalClientBerichtMagOpnieuwVerzondenWorden(getModel().getObject()));

		return container;
	}

	private Form<Void> getEmailFormulier()
	{
		Form<Void> emailFormulier = new Form<>("emailForm");
		emailFormulier.add(getEmailTekstveld());
		emailFormulier.add(new AjaxSubmitLink("herzenden")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				herzendEmail();
				info(getString("info.mailherzonden"));
				target.add(emailFormulier);
			}
		});
		return emailFormulier;
	}

	private void herzendEmail()
	{
		if (Bevolkingsonderzoek.MAMMA == bepaalBevolkingsonderzoek())
		{
			mammaDigitaalContactService.herzendBevestigAfspraakMail((MammaDigitaalClientBericht) getModel().getObject(), nieuwEmailadresModel.getObject(),
				ScreenitSession.get().getLoggedInAccount());
		}
		else
		{
			throw new IllegalStateException();
		}
	}

	private TextField<String> getEmailTekstveld()
	{
		var voorkeurmailClient = getModel().getObject().getScreeningRonde().getDossier().getClient().getPersoon().getEmailadres();
		nieuwEmailadresModel = new Model<>(voorkeurmailClient);
		TextField<String> emailVeld = new TextField<>("E-mailadres", nieuwEmailadresModel);
		setEmailValidaties(emailVeld);
		return emailVeld;
	}

	private void setEmailValidaties(TextField<String> emailVeld)
	{
		emailVeld.add(StringValidator.maximumLength(GbaPersoon.MAX_EMAIL_LENGTH));
		emailVeld.add(EmailAddressValidator.getInstance());
		emailVeld.setRequired(true);
	}

	private Bevolkingsonderzoek bepaalBevolkingsonderzoek()
	{
		return getModel().getObject().getScreeningRonde().getBevolkingsonderzoek();
	}

}
