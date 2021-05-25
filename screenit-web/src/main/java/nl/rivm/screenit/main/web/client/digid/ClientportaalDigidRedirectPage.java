package nl.rivm.screenit.main.web.client.digid;

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

import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.service.InstellingService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.spring.injection.annot.SpringBean;

import com.google.common.base.Strings;

public class ClientportaalDigidRedirectPage extends WebPage
{

	private static final Logger LOG = LoggerFactory.getLogger(ClientportaalDigidRedirectPage.class);

	private static final long serialVersionUID = 1L;

	@SpringBean
	private InstellingService instellingService;

	public ClientportaalDigidRedirectPage(final PageParameters parameters)
	{
		LOG.info("Pagina " + this.getClass().getName() + " wordt geladen");
		String parameterRegioCode = parameters.get("regiocode").toString();
		String parameterUrl = parameters.get("url").toString();
		String url = null;

		if (!Strings.isNullOrEmpty(parameterRegioCode))
		{
			List<ScreeningOrganisatie> list = instellingService.getAllActiefScreeningOrganisaties();
			for (ScreeningOrganisatie screen : list)
			{
				if (screen.getRegioCode() != null)
				{
					screen.getRegioCode().equalsIgnoreCase(parameterRegioCode);
					if (screen.getWebsite() != null)
					{
						url = screen.getWebsite();
						break;
					}
				}
			}
		}

		if (!Strings.isNullOrEmpty(parameterUrl))
		{
			if (parameterUrl.startsWith("http"))
			{
				url = parameterUrl;
			}
			else
			{
				url = "http://" + parameterUrl;
			}
		}
		ScreenitSession.get().setAfkomstigURLRegioCode(url);
		setStatelessHint(true);
		setResponsePage(ScreenitDigidLoginPage.class);
	}

}
