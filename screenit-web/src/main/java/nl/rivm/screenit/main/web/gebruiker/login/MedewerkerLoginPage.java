package nl.rivm.screenit.main.web.gebruiker.login;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.ApplicationEnvironment;

import org.apache.wicket.spring.injection.annot.SpringBean;

public class MedewerkerLoginPage extends LoginBasePage
{
	private static final long serialVersionUID = 1L;

	@SpringBean(name = "applicationEnvironment")
	private String applicationEnvironment;

	public MedewerkerLoginPage()
	{
		if (ApplicationEnvironment.PROD.getEnvNaam().equalsIgnoreCase(applicationEnvironment) ||
			ApplicationEnvironment.PAT.getEnvNaam().equalsIgnoreCase(applicationEnvironment) ||
			ApplicationEnvironment.OPL.getEnvNaam().equalsIgnoreCase(applicationEnvironment))
		{
			setResponsePage(MedewerkerLoginMethodPage.class);
		}
		else
		{
			add(new MedewerkerLoginPanel("panel"));
		}
	}
}
