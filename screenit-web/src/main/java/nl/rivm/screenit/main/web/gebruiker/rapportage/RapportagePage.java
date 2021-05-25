package nl.rivm.screenit.main.web.gebruiker.rapportage;

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

import nl.rivm.screenit.main.service.OpenIDConncetIdpService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerHoofdMenuItem;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.link.ExternalLink;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	constraint = ShiroConstraint.HasPermission,
	level = ToegangLevel.EIGEN,
	recht = { Recht.GEBRUIKER_RAPPORTAGE, Recht.GEBRUIKER_RAPPORTAGE_WEBFOCUS },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class RapportagePage extends GebruikerBasePage
{

	@SpringBean(name = "ultimviewSsoUrl")
	private String ultimviewSsoUrl;

	@SpringBean
	private OpenIDConncetIdpService openIDConncetIdpService;

	public RapportagePage()
	{
		ExternalLink ssoLink = new ExternalLink("openSso", ultimviewSsoUrl);
		ssoLink.setEnabled(StringUtils.isNotBlank(ultimviewSsoUrl));
		ssoLink.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_RAPPORTAGE, Actie.INZIEN));
		add(ssoLink);

		IndicatingAjaxLink<Void> webFocusLink = new IndicatingAjaxLink<Void>("openSsoWebFocus")
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				final String ssoUrl = openIDConncetIdpService
					.createWebFocusSsoUrl(ScreenitSession.get().getLoggedInInstellingGebruiker());
				final String javaScriptString = String.format("window.open('%s', '_blank')", ssoUrl);
				target.appendJavaScript(javaScriptString);
			}

		};
		webFocusLink.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_RAPPORTAGE_WEBFOCUS, Actie.INZIEN));

		webFocusLink.setOutputMarkupPlaceholderTag(true);
		add(webFocusLink);
	}

	@Override
	protected GebruikerHoofdMenuItem getActieveMenuItem()
	{
		return GebruikerHoofdMenuItem.RAPPORTAGE;
	}
}
