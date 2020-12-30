package nl.rivm.screenit.main.web.gebruiker.gedeeld;

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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;

import nl.rivm.screenit.service.mamma.MammaBaseDossierService;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaDoelgroepIndicatorPanel extends Panel
{

	@SpringBean
	private MammaBaseDossierService baseDossierService;

	public MammaDoelgroepIndicatorPanel(String id, MammaDossier dossier, boolean toonTehuis)
	{
		super(id);
		boolean isMinderValideClient = false;
		boolean isDubbeleTijdClient = false;
		boolean isTehuisClient = false;
		boolean isSuspect = false;
		if (dossier != null)
		{
			MammaDoelgroep doelgroep = dossier.getDoelgroep();
			isMinderValideClient = MammaDoelgroep.MINDER_VALIDE.equals(doelgroep);
			isDubbeleTijdClient = MammaDoelgroep.DUBBELE_TIJD.equals(doelgroep);
			isTehuisClient = dossier.getTehuis() != null;
			isSuspect = baseDossierService.isSuspect(dossier);
		}
		add(new WebMarkupContainer("dubbleTijd").setVisible(isDubbeleTijdClient));
		add(new WebMarkupContainer("minderValide").setVisible(isMinderValideClient));
		add(new WebMarkupContainer("tehuis").setVisible(isTehuisClient && toonTehuis));
		add(new WebMarkupContainer("suspect").setVisible(isSuspect));

		setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_DOSSIERGEGEVENS, Actie.INZIEN)
			|| ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_AFSPRAKEN_BEHEER, Actie.INZIEN)
			|| ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_TEHUIS, Actie.INZIEN));
	}

}
