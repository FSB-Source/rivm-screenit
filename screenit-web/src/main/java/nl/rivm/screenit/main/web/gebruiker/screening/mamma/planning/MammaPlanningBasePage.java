
package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning;

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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.MammaScreeningBasePage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.blokkade.MammaBlokkadeBeheerPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.dashboard.MammaPlanningDashboardPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.screeningseenheid.MammaSEZoekenPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.standplaats.MammaStandplaatsZoekenPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.tehuis.MammaTehuisZoekenPage;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;

import org.apache.wicket.Component;

public abstract class MammaPlanningBasePage extends MammaScreeningBasePage
{

	private static final long serialVersionUID = 1L;

	protected boolean magAanpassen;

	protected boolean ingelogdNamensRegio;

	public MammaPlanningBasePage()
	{
		super();
		ScreeningOrganisatie sessionSO = ScreenitSession.get().getScreeningOrganisatie();
		ingelogdNamensRegio = sessionSO != null;
		magAanpassen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING, Actie.AANPASSEN) && ingelogdNamensRegio;
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<>();
		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.planning.dashboard", MammaPlanningDashboardPage.class));
		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.planning.blokkade.beheer", MammaBlokkadeBeheerPage.class));
		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.planning.standplaats", MammaStandplaatsZoekenPage.class));
		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.planning.postcodereeks", MammaPostcodeReeksZoekenPage.class));
		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.planning.se", MammaSEZoekenPage.class));
		contextMenuItems.add(new GebruikerMenuItem("label.tab.mammascreening.planning.tehuis", MammaTehuisZoekenPage.class));
		return contextMenuItems;
	}

	@Override
	protected Component maakContextMenuExtensie(String id)
	{
		return new MammaPlanningTabExtensiePanel(id);
	}

	@Override
	protected boolean bevatFormulieren()
	{
		return Boolean.TRUE;
	}
}
