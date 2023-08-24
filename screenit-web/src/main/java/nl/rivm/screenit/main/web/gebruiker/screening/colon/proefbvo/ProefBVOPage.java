
package nl.rivm.screenit.main.web.gebruiker.screening.colon.proefbvo;

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
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;

import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_AFMELDEN_PROEF_BEVOLKINGSONDERZOEK,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class ProefBVOPage extends ColonScreeningBasePage
{

	private static final long serialVersionUID = 1L;

	public ProefBVOPage()
	{

	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		return createContextMenu();
	}

	private List<GebruikerMenuItem> createContextMenu()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<GebruikerMenuItem>();
		contextMenuItems.add(new GebruikerMenuItem("menu.proefbvo.afmelden", ProefBVOAfmeldenPage.class));
		contextMenuItems.add(new GebruikerMenuItem("menu.proefbvo.heraanmelden", ProefBVOHeraanmeldenPage.class));
		return contextMenuItems;
	}

}
