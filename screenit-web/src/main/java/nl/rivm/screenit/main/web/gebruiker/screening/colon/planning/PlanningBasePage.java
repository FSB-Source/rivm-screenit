
package nl.rivm.screenit.main.web.gebruiker.screening.colon.planning;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.ColonScreeningBasePage;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.planning.blokkadesview.BlokkadeListViewPage;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.planning.listview.RoosterListViewPage;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.planning.rooster.RoosterPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;

import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_LOCATIE_ROOSTER,
	organisatieTypeScopes = OrganisatieType.COLOSCOPIECENTRUM,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class PlanningBasePage extends ColonScreeningBasePage
{

	private static final long serialVersionUID = 1L;

	public PlanningBasePage()
	{

	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<GebruikerMenuItem>();
		contextMenuItems.add(new GebruikerMenuItem("label.planning.rooster", RoosterPage.class));
		contextMenuItems.add(new GebruikerMenuItem("label.planning.roosterblok.listview", RoosterListViewPage.class));
		contextMenuItems.add(new GebruikerMenuItem("label.planning.blokkade.listview", BlokkadeListViewPage.class));
		return contextMenuItems;
	}
}
