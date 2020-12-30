
package nl.rivm.screenit.main.web.gebruiker.login;

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

import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.GebruikersService;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.password.web.component.WachtwoordValidator;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.HiddenField;
import org.apache.wicket.markup.html.form.PasswordTextField;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class PasswordChangePanel extends GenericPanel<Gebruiker>
{

	private static final long serialVersionUID = 1L;

	private String wachtwoord1;

	private String wachtwoord2;

	@SpringBean
	private LogService logService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private GebruikersService gebruikersService;

	public PasswordChangePanel(String id, Gebruiker gebruiker)
	{
		super(id, ModelUtil.cRModel(gebruiker));

		ScreenitForm<InstellingGebruiker> passwordChangeForm = new ScreenitForm<InstellingGebruiker>("passwordChangeForm");

		HiddenField<String> gebruikersnaam = new HiddenField<>("gebruikersnaam");
		passwordChangeForm.add(gebruikersnaam);
		PasswordTextField wachtwoord1TextField = new PasswordTextField("wachtwoord1");
		passwordChangeForm.add(wachtwoord1TextField);
		wachtwoord1TextField.setOutputMarkupId(true);
		wachtwoord1TextField.setModel(new PropertyModel<String>(this, "wachtwoord1"));
		wachtwoord1TextField.add(new WachtwoordValidator(gebruikersnaam, true));
		wachtwoord1TextField.setLabel(Model.of("Wachtwoord"));

		PasswordTextField wachtwoord2TextField = new PasswordTextField("wachtwoord2");
		passwordChangeForm.add(wachtwoord2TextField);
		wachtwoord2TextField.setOutputMarkupId(true);
		wachtwoord2TextField.setModel(new PropertyModel<String>(this, "wachtwoord2"));
		wachtwoord1TextField.setLabel(Model.of("Wachtwoord controle"));

		AjaxSubmitLink opslaan = new AjaxSubmitLink("opslaan")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				Gebruiker medewerker = PasswordChangePanel.this.getModelObject();

				String previousPassword = medewerker.getWachtwoord();
				Gebruiker newPasswordMedewerker = new Gebruiker();
				newPasswordMedewerker.setId(medewerker.getId());
				gebruikersService.setWachtwoord(newPasswordMedewerker, wachtwoord1);
				String newPassword = newPasswordMedewerker.getWachtwoord();

				if (wachtwoord1 != null && !wachtwoord1.equals(wachtwoord2))
				{
					error(getLocalizer().getString("error.password.notequals", this));
				}
				else if (previousPassword.equals(newPassword))
				{
					error(getLocalizer().getString("error.password.equalsprevious", this));
				}
				else if (StringUtils.isNotEmpty(wachtwoord1) && wachtwoord1.equals(wachtwoord2))
				{

					gebruikersService.setWachtwoord(medewerker, wachtwoord1);

					hibernateService.saveOrUpdate(medewerker);
					logService.logGebeurtenis(LogGebeurtenis.WACHTWOORD_GEWIJZIGD, medewerker);

					onWachtwoordChanged(target, medewerker);
				}
			}

		};
		passwordChangeForm.add(opslaan);

		passwordChangeForm.setDefaultButton(opslaan);

		add(passwordChangeForm);
	}

	public String getWachtwoord1()
	{
		return this.wachtwoord1;
	}

	public void setWachtwoord1(String wachtwoord1)
	{
		this.wachtwoord1 = wachtwoord1;
	}

	public String getWachtwoord2()
	{
		return this.wachtwoord2;
	}

	public void setWachtwoord2(String wachtwoord2)
	{
		this.wachtwoord2 = wachtwoord2;
	}

	protected abstract void onWachtwoordChanged(AjaxRequestTarget target, Gebruiker gebruiker);
}
