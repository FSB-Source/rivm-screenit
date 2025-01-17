
package nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie;

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

import nl.rivm.screenit.main.model.Parameterisatie;
import nl.rivm.screenit.main.service.ParameterisatieService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;

import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	recht = Recht.GEBRUIKER_BEHEER_PARAMETERISATIE,
	actie = Actie.INZIEN,
	level = ToegangLevel.REGIO,
	bevolkingsonderzoekScopes = Bevolkingsonderzoek.CERVIX,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission)
public class CervixParameterisatiePage extends ParameterisatieBasePage
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private ParameterisatieService parameterisatieService;

	public CervixParameterisatiePage()
	{
		Parameterisatie parameterisatie = parameterisatieService.loadParameterisatie();
		add(new CervixPrimaireParametersPanel("landelijkeParameters", new Model<>(parameterisatie)));

		if (ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CERVIX_HUISARTS_TARIEF, Actie.INZIEN))
		{
			add(new CervixHuisartsTarievenPanel("huisartsTarieven"));
		}
		else
		{
			add(new EmptyPanel("huisartsTarieven"));
		}
	}
}
