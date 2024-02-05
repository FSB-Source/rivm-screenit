package nl.rivm.screenit.main.web.gebruiker.login;

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

import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.ScreenitWachtwoordField;
import nl.rivm.screenit.main.web.component.validator.ScreenITWachtwoordValidator;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.WachtwoordService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.form.HiddenField;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class PasswordChangePanel extends GenericPanel<Gebruiker>
{

	@SpringBean
	private LogService logService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private WachtwoordService wachtwoordService;

	private String wachtwoord1;

	private String wachtwoord2;

	public PasswordChangePanel(String id, Gebruiker gebruiker)
	{
		super(id, ModelUtil.cRModel(gebruiker));

		ScreenitForm<InstellingGebruiker> passwordChangeForm = new ScreenitForm<>("passwordChangeForm");

		HiddenField<String> gebruikersnaam = new HiddenField<>("gebruikersnaam");
		passwordChangeForm.add(gebruikersnaam);

		ScreenITWachtwoordValidator validator = new ScreenITWachtwoordValidator(gebruikersnaam, true, getModel());
		passwordChangeForm.add(new ScreenitWachtwoordField("wachtwoord1", new PropertyModel<>(this, "wachtwoord1"), true, validator));
		passwordChangeForm.add(new ScreenitWachtwoordField("wachtwoord2", new PropertyModel<>(this, "wachtwoord2"), true, null));

		AjaxSubmitLink opslaan = new AjaxSubmitLink("opslaan")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				Gebruiker medewerker = PasswordChangePanel.this.getModelObject();

				String previousPassword = medewerker.getWachtwoord();
				Gebruiker newPasswordMedewerker = new Gebruiker();
				newPasswordMedewerker.setId(medewerker.getId());
				wachtwoordService.setWachtwoord(newPasswordMedewerker, wachtwoord1);
				String newPassword = newPasswordMedewerker.getWachtwoord();

				if (wachtwoord1 != null && !wachtwoord1.equals(wachtwoord2))
				{
					error(getLocalizer().getString("error.password.notequals", this));
				}
				else if (previousPassword.equals(newPassword))
				{
					error(getLocalizer().getString("error.password.equalsprevious", this));
				}
				else if (StringUtils.isNotEmpty(wachtwoord1))
				{

					wachtwoordService.setWachtwoord(medewerker, wachtwoord1);

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

	protected abstract void onWachtwoordChanged(AjaxRequestTarget target, Gebruiker gebruiker);
}
