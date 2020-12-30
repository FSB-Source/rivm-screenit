package nl.rivm.screenit.main.web.client.dashboard.cervix;

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

import nl.rivm.screenit.main.web.client.base.ClientHoofdMenuitem;
import nl.rivm.screenit.main.web.client.dashboard.ClientBaseDashboardVervangendeTekstPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;

import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.CLIENT_DASHBOARD,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.CERVIX })
public class ClientCervixDashboardVervangendeTekstPage extends ClientBaseDashboardVervangendeTekstPage
{
	private static final long serialVersionUID = 1L;

	public ClientCervixDashboardVervangendeTekstPage()
	{
		super(Bevolkingsonderzoek.CERVIX);
	}

	@Override
	public ClientHoofdMenuitem getHoofdMenuitem()
	{
		return ClientHoofdMenuitem.MIJNBEVOLKINGSONDERZOEK_CERVIX;
	}
}
