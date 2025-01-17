
package nl.rivm.screenit.main.web.gebruiker.algemeen;

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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerHoofdMenuItem;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;

public class AlgemeenPage extends GebruikerBasePage
{

	private static final long serialVersionUID = 1L;

	public AlgemeenPage()
	{

	}

	protected void setCurrentSelectedMedewerker(Gebruiker medewerker)
	{
		ScreenitSession.get().setCurrentSelectedMedewerker(medewerker);
	}

	protected Gebruiker getCurrentSelectedMedewerker()
	{
		return ScreenitSession.get().getCurrentSelectedMedewerker();
	}

	protected void setCurrentSelectedOrganisatie(Instelling organisatie)
	{
		ScreenitSession.get().setCurrentSelectedOrganisatie(organisatie);
	}

	protected Instelling getCurrentSelectedOrganisatie()
	{
		return ScreenitSession.get().getCurrentSelectedOrganisatie();
	}

	@Override
	protected GebruikerHoofdMenuItem getActieveMenuItem()
	{
		return GebruikerHoofdMenuItem.ALGEMEEN;
	}

}
