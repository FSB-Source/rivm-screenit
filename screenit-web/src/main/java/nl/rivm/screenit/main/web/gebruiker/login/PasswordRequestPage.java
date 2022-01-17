package nl.rivm.screenit.main.web.gebruiker.login;

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

import java.util.Map;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.AuthenticatieService;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateSearchService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.behavior.FocusBehavior;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.spring.injection.annot.SpringBean;

@Slf4j
public class PasswordRequestPage extends LoginBasePage
{
	@SpringBean
	private AuthenticatieService authenticatieService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private HibernateSearchService hibernateSearchService;

	public PasswordRequestPage(PageParameters pageParameters)
	{
		Gebruiker medewerker = new Gebruiker();
		if (pageParameters != null && pageParameters.get("naam") != null && !pageParameters.get("naam").equals("null"))
		{
			medewerker.setGebruikersnaam(pageParameters.get("naam").toString());
		}
		final Form<Gebruiker> form = new Form<>("requestForm");
		form.setDefaultModel(ModelUtil.csModel(medewerker));

		ComponentHelper.addTextField(form, "gebruikersnaam", false, 50, false).add(new FocusBehavior());
		ComponentHelper.addTextField(form, "emailextra", false, 50, false);

		AjaxButton zoeken = new AjaxButton("zoek", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				Gebruiker gebruiker = form.getModelObject();
				if (StringUtils.isBlank(gebruiker.getGebruikersnaam()) && StringUtils.isBlank(gebruiker.getEmailextra()))
				{
					error(getLocalizer().getString("error.password.request", this));
					return;
				}
				int size = hibernateSearchService.count(gebruiker);

				info(getLocalizer().getString("info.gegevens.verstuurd", this));
				if (size == 1)
				{
					requestPassword(gebruiker);
				}
			}

			@Override
			protected void onError(AjaxRequestTarget target)
			{
				super.onError(target);
				error(getLocalizer().getString("error.password.request", this));
			}

		};
		form.add(zoeken);
		form.setDefaultButton(zoeken);
		add(form);

	}

	private void requestPassword(Gebruiker medewerkerModelObject)
	{
		Map<Gebruiker, Boolean> medewerkerMap = authenticatieService.requestNewPassword(medewerkerModelObject);

		Gebruiker medewerker = null;
		if (medewerkerMap.size() > 0)
		{
			medewerker = medewerkerMap.keySet().iterator().next();
		}

		if (medewerker != null)
		{
			if (medewerker.getEmailwerk() == null && medewerker.getEmailextra() == null)
			{
				logService.logGebeurtenis(LogGebeurtenis.AANVRAGEN_ACCOUNT_MISLUKT, medewerker, "Er is geen mail adres aanwezig voor deze gebruiker");
			}
			else
			{
				logService.logGebeurtenis(LogGebeurtenis.WACHTWOORD_AANGEVRAAGD, medewerker);
			}
		}
	}
}
