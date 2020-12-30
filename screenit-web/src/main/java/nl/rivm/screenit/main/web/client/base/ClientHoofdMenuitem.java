
package nl.rivm.screenit.main.web.client.base;

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

import nl.rivm.screenit.main.web.client.dashboard.cervix.ClientCervixDashboardPage;
import nl.rivm.screenit.main.web.client.dashboard.colon.ClientColonDashboardPage;
import nl.rivm.screenit.main.web.client.dashboard.home.ClientHomeDashboardPage;
import nl.rivm.screenit.main.web.client.dashboard.mamma.ClientMammaDashboardPage;

public enum ClientHoofdMenuitem
{
	MIJNBEVOLKINGSONDERZOEK(ClientHomeDashboardPage.class),

	MIJNBEVOLKINGSONDERZOEK_CERVIX(ClientCervixDashboardPage.class),

	MIJNBEVOLKINGSONDERZOEK_MAMMA(ClientMammaDashboardPage.class),

	MIJNBEVOLKINGSONDERZOEK_COLON(ClientColonDashboardPage.class);

	private Class<? extends ClientBasePage> targetClass;

	private ClientHoofdMenuitem(Class<? extends ClientBasePage> targetClass)
	{
		this.targetClass = targetClass;
	}

	public Class<? extends ClientBasePage> getTargetClass()
	{
		return targetClass;
	}
}
