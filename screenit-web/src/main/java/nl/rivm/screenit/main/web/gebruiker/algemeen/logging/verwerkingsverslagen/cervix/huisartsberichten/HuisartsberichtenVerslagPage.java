package nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.cervix.huisartsberichten;

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

import nl.rivm.screenit.main.web.gebruiker.algemeen.AlgemeenPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.logging.LoggingInzienPage;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.verwerkingverslag.cervix.CervixHuisartsberichtenRapportage;

import org.apache.wicket.model.IModel;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(actie = Actie.INZIEN, checkScope = false, constraint = ShiroConstraint.HasPermission, recht = Recht.GEBRUIKER_HUISARTSBERICHTEN_VERSLAG, bevolkingsonderzoekScopes = Bevolkingsonderzoek.CERVIX)
public class HuisartsberichtenVerslagPage extends AlgemeenPage
{

	private static final long serialVersionUID = 1L;

	public HuisartsberichtenVerslagPage(IModel<CervixHuisartsberichtenRapportage> model)
	{
		add(new HuisartsberichtenVerslagPanel("panel", model));
	}

	@Override
	protected Class<? extends GebruikerBasePage> getActiveSubMenuClass()
	{
		return LoggingInzienPage.class;
	}
}
