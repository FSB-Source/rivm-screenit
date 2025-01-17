package nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.MammaScreeningBasePage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.geenbeoordelingmogelijk.MammaCeGeenBeoordelingMogelijkWerklijstPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.onderbroken.MammaCeOnderbrokenOnderzoekenWerklijstPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.opschorting.MammaCeOpgeschorteBeoordelingenWerklijstPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.procesmonitoring.MammaCeProcesmonitoringWerklijstPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.uploadbeeldenverzoeken.MammaCeUploadBeeldenVerzoekInstellingWerklijstPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.uploadbeeldenverzoeken.MammaCeUploadBeeldenVerzoekWerklijstPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.verwijsverslag.MammaCeVerwijsVerslagenWerklijstPage;

public abstract class AbstractMammaCePage extends MammaScreeningBasePage
{

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		return getContextMenuItemsList();
	}

	public static List<GebruikerMenuItem> getContextMenuItemsList()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<>();
		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.ce-verwijsverslagen-werklijst",
				MammaCeVerwijsVerslagenWerklijstPage.class));
		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.ce-opgeschort-be-verslagen-werklijst",
				MammaCeOpgeschorteBeoordelingenWerklijstPage.class));
		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.ce-procesmonitoring-werklijst",
				MammaCeProcesmonitoringWerklijstPage.class));
		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.ce-geen-beoordeling-mogelijk-werklijst",
				MammaCeGeenBeoordelingMogelijkWerklijstPage.class));
		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.ce-onderbroken-onderzoeken-werklijst",
				MammaCeOnderbrokenOnderzoekenWerklijstPage.class));
		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.ce-openstaande-uploadverzoeken-werklijst",
				MammaCeUploadBeeldenVerzoekWerklijstPage.class));

		return contextMenuItems;
	}

}
