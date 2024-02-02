package nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.MammaScreeningBasePage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.followup.MammaFollowUpNietGedownloadWerklijst;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.followupconclusie.MammaFollowUpConclusieWerklijst;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.followuppathologie.MammaFollowUpPathologieRegioWerklijstPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.followupradiologie.MammaFollowUpRadiologieRegioWerklijstPage;

import java.util.ArrayList;
import java.util.List;

public class AbstractMammaFollowUpPage extends MammaScreeningBasePage
{
	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		return getContextMenuItemsList();
	}

	public static List<GebruikerMenuItem> getContextMenuItemsList()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<>();
		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.followup-niet-gedownload-werklijst",
			MammaFollowUpNietGedownloadWerklijst.class));
		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.followup-radiologie-werklijst", MammaFollowUpRadiologieRegioWerklijstPage.class));
		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.followup-pathologie-werklijst", MammaFollowUpPathologieRegioWerklijstPage.class));
		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.followup-conclusie-werklijst", MammaFollowUpConclusieWerklijst.class));

		return contextMenuItems;
	}

}
