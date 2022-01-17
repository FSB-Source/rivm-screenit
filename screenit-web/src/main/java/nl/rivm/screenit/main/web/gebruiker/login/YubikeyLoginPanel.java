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

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ScreenitWachtwoordField;
import nl.rivm.screenit.model.enums.InlogMethode;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.bootstrap.BootstrapFeedbackPanel;
import nl.topicuszorg.wicket.input.behavior.FocusBehavior;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxSelfUpdatingTimerBehavior;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.PasswordTextField;
import org.apache.wicket.markup.html.form.SubmitLink;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.link.StatelessLink;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.time.Duration;

public class YubikeyLoginPanel extends Panel
{

	@SpringBean
	private SimplePreferenceService preferenceService;

	public YubikeyLoginPanel(String id)
	{
		super(id);
		add(new LoginForm("loginForm"));
	}

	public final class LoginForm extends Form<Void>
	{
		private String gebruikersnaam;

		private String wachtwoord;

		private String yubikeyOTP;

		public LoginForm(final String id)
		{
			super(id);

			add(new BootstrapFeedbackPanel("feedback"));

			add(new TextField<>("gebruikersnaam", new PropertyModel<>(this, "gebruikersnaam")).add(new FocusBehavior()));
			add(new ScreenitWachtwoordField("wachtwoord", new PropertyModel<>(this, "wachtwoord"), false, null));
			add(new PasswordTextField("yubikeyOTP", new PropertyModel<>(this, "yubikeyOTP")));

			Boolean aanvragenwachtwoordVisible = preferenceService.getBoolean(PreferenceKey.WACHTWOORDAANVRAGEN.name());
			if (aanvragenwachtwoordVisible == null)
			{
				aanvragenwachtwoordVisible = Boolean.FALSE;
			}

			StatelessLink<Void> passwordRequest = new StatelessLink<>("passwordRequest")
			{
				@Override
				public void onClick()
				{
					PageParameters parameters = new PageParameters();
					if (StringUtils.isNotBlank(gebruikersnaam))
					{
						parameters.add("naam", gebruikersnaam);
					}
					setResponsePage(PasswordRequestPage.class, parameters);
				}

			};
			passwordRequest.setVisible(aanvragenwachtwoordVisible);
			add(passwordRequest);

			add(new SubmitLink("submit")
			{
				@Override
				public void onSubmit()
				{

					ScreenitSession session = ScreenitSession.get();
					session.replaceSession();
					String yubiOTP = yubikeyOTP;
					if (yubiOTP != null && yubiOTP.length() > 32)
					{
						yubiOTP = yubiOTP.substring(yubiOTP.length() - 32);
					}
					Component loginResult = session.login(gebruikersnaam, wachtwoord, yubiOTP);

					if (loginResult instanceof WebPage)
					{

						setResponsePage((WebPage) loginResult);
						InlogMethodCookie.setCookie(InlogMethode.YUBIKEY);
					}
					else if (session.getFeedbackMessages().isEmpty())
					{
						error(getString("error.nietvoldoende.rechten"));
					}
				}

			});

			add(new WebMarkupContainer("keepalive").add(new AjaxSelfUpdatingTimerBehavior(Duration.minutes(10))));

		}
	}
}
