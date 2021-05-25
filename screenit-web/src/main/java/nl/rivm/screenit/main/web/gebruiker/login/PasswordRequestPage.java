package nl.rivm.screenit.main.web.gebruiker.login;

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

import java.util.Map;

import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
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
import org.apache.wicket.protocol.http.servlet.ServletWebRequest;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PasswordRequestPage extends LoginBasePage
{

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(PasswordRequestPage.class);

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
		final Form<Gebruiker> form = new Form<Gebruiker>("requestForm");
		form.setDefaultModel(ModelUtil.cRModel(medewerker));

		ComponentHelper.addTextField(form, "gebruikersnaam", false, 50, false).add(new FocusBehavior());
		ComponentHelper.addTextField(form, "emailextra", false, 50, false);

		AjaxButton zoeken = new AjaxButton("zoek", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				ServletWebRequest request = (ServletWebRequest) getRequest();

				Gebruiker gebruiker = (Gebruiker) form.getModelObject();
				if (StringUtils.isBlank(gebruiker.getGebruikersnaam()) && StringUtils.isBlank(gebruiker.getEmailextra()))
				{
					error(getLocalizer().getString("error.password.request", this));
					return;
				}
				int size = hibernateSearchService.count(gebruiker);

				if (size == 1)
				{
					requestPassword(request, target, gebruiker);
				}
				else
				{
					error(getLocalizer().getString("error.password.request.wrong", this));
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

	private void requestPassword(ServletWebRequest request, AjaxRequestTarget target, Gebruiker medewerkerModelObject)
	{
		Map<Gebruiker, Boolean> medewerkerMap = authenticatieService.requestNewPassword(medewerkerModelObject);

		boolean gelukt = false;

		Gebruiker medewerker = null;
		if (medewerkerMap.size() > 0)
		{
			medewerker = medewerkerMap.keySet().iterator().next();

			gelukt = medewerkerMap.get(medewerker);
		}

		if (medewerker == null)
		{
			error(getLocalizer().getString("error.noUser", this));

		}
		else if (medewerker.getEmailwerk() == null && medewerker.getEmailextra() == null)
		{
			logService.logGebeurtenis(LogGebeurtenis.AANVRAGEN_ACCOUNT_MISLUKT, medewerker, "Er is geen mail adres aanwezig voor deze gebruiker");

			error(getLocalizer().getString("error.login.no.email", this));
		}
		else if (gelukt)
		{
			logService.logGebeurtenis(LogGebeurtenis.WACHTWOORD_AANGEVRAAGD, medewerker);
			info(getLocalizer().getString("infoSend", this));
		}
		else
		{
			String gebruikteAdres = medewerker.getEmailwerk();
			if (StringUtils.isNotBlank(medewerker.getEmailextra()))
			{
				gebruikteAdres = medewerker.getEmailextra();
			}

			logService.logGebeurtenis(LogGebeurtenis.AANVRAGEN_ACCOUNT_MISLUKT, medewerker, "Het gebruikte mail adres: " + gebruikteAdres, Bevolkingsonderzoek.COLON);

			error(getLocalizer().getString("error.login.non.existing.email", this));
		}
	}
}
