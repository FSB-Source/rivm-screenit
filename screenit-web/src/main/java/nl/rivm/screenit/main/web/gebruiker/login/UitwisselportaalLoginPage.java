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

import nl.rivm.screenit.main.web.gebruiker.login.uzipas.UzipasLoginMethodPage;

import org.apache.wicket.request.mapper.parameter.PageParameters;

public class UitwisselportaalLoginPage extends LoginBasePage
{

	public UitwisselportaalLoginPage(PageParameters pageParameters)
	{
		this();
	}

	public UitwisselportaalLoginPage()
	{
		PageParameters parameters = new PageParameters();
		parameters.add(PAGE_PARAMETER_UITWISSELPORTAAL, Boolean.TRUE);
		setResponsePage(UzipasLoginMethodPage.class, parameters);
	}

	@Override
	protected void goToBeginScherm()
	{
		setResponsePage(UitwisselportaalLoginPage.class);
	}
}
