package nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.web.gebruiker.algemeen.AlgemeenPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie.huisartsberichten.HuisartsBerichtTemplateEditPage;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.model.enums.Actie;

public abstract class ParameterisatieBasePage extends AlgemeenPage
{

	private static final long serialVersionUID = 1L;

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{

		List<GebruikerMenuItem> contextMenuItems = new ArrayList<GebruikerMenuItem>();
		contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.parameterisatie.darmkanker", ColonParameterisatiePage.class));
		contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.parameterisatie.baarmoederhalskanker", CervixParameterisatiePage.class));
		contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.parameterisatie.borstkanker", MammaParameterisatiePage.class));
		contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.parameterisatie.clientportaal", ClientportaalParameterisatiePage.class));
		contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.parameterisatie.wachtwoordconfiguratie", WachtwoordConfiguratiePage.class));
		contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.parameterisatie.medewerkerconfiguratie", EmailConfiguratiePage.class));
		contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.parameterisatie.overeenkomstconfiguratie", OvereenkomstConfiguratiePage.class));
		contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.parameterisatie.huisartsberichttemplate", HuisartsBerichtTemplateEditPage.class));
		contextMenuItems.add(new GebruikerMenuItem("menu.algemeen.parameterisatie.bewaartermijnen", BewaartermijnenParameterisatiePage.class));
		return contextMenuItems;
	}

	@Override
	protected boolean bevatFormulieren()
	{
		return Boolean.TRUE;
	}

	@Override
	protected boolean isMinimumActie(Actie actie, Actie minimaal)
	{
		return Actie.INZIEN.equals(minimaal) && actie == null || actie != null && actie.getNiveau() >= minimaal.getNiveau();
	}
}
