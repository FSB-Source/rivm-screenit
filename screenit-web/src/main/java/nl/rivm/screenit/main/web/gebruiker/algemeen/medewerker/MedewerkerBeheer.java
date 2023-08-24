
package nl.rivm.screenit.main.web.gebruiker.algemeen.medewerker;

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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.algemeen.AlgemeenPage;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;

public abstract class MedewerkerBeheer extends AlgemeenPage
{

	private static final long serialVersionUID = 1L;

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		return createContextMenu();
	}

	@Override
	protected boolean bevatFormulieren()
	{

		return Boolean.TRUE;
	}

	public static List<GebruikerMenuItem> createContextMenu()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<GebruikerMenuItem>();
		contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.medewerkers.zoeken", MedewerkerZoeken.class));
		contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.medewerkers.gegevens", MedewerkerBasisgegevens.class));
		if (ScreenitSession.get().getCurrentSelectedMedewerker() != null)
		{
			contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.medewerkers.overeenkomsten", MedewerkerOvereenkomstenPage.class));
			contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.medewerkers.organisaties", MedewerkerKoppelPage.class));

		}

		return contextMenuItems;
	}

}
