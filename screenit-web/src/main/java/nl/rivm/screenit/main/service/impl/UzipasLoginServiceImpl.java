package nl.rivm.screenit.main.service.impl;

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

import java.net.MalformedURLException;
import java.net.URL;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.service.UzipasLoginService;
import nl.rivm.screenit.main.web.ScreenitApplication;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class UzipasLoginServiceImpl implements UzipasLoginService
{
	@Autowired
	@Qualifier(value = "applicationUrl")
	private String applicationUrl;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Override
	public URL uziLoginUrl(boolean fromUitwisselportaal)
	{
		try
		{
			URL url = new URL(applicationUrlWithFinalSlash(fromUitwisselportaal) + ScreenitApplication.CHECK_UZIPAS_MOUNT);
			String uziSubdomein = preferenceService.getString(PreferenceKey.INTERNAL_UZI_LOGIN_URL_PREFIX.name(), "");
			return new URL(url.getProtocol(), uziSubdomein + url.getHost(), url.getPort(), url.getFile());
		}
		catch (MalformedURLException e)
		{
			throw new IllegalStateException("Ongeldige UZI login URL", e);
		}
	}

	@Override
	public String applicationUrl(boolean fromUitwisselportaal)
	{
		return StringUtils.chop(applicationUrlWithFinalSlash(fromUitwisselportaal));
	}

	private String applicationUrlWithFinalSlash(boolean fromUitwisselportaal)
	{
		String urlString = applicationUrl;
		if (fromUitwisselportaal)
		{
			urlString = applicationUrl.replace(Constants.BASE_SUBDOMEIN_MEDEWERKERPORTAAL, ScreenitApplication.UITWISSELPORTAAL_MOUNT);
		}
		if (!urlString.endsWith("/"))
		{
			urlString += "/";
		}
		return urlString;
	}

	@Override
	public URL uziLoginUrlRoot(boolean fromUitwisselportaal)
	{
		try
		{
			URL uziLoginUrl = uziLoginUrl(fromUitwisselportaal);
			return new URL(uziLoginUrl.getProtocol(), uziLoginUrl.getHost(), uziLoginUrl.getPort(), "");
		}
		catch (MalformedURLException e)
		{
			throw new IllegalStateException("Ongeldige UZI login URL", e);
		}
	}
}
