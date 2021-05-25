package nl.rivm.screenit.main.web.gebruiker.screening.cervix.labformulier.controleren;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.labformulier.werklijst.CervixLabformulierenControlerenPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;

import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(constraint = ShiroConstraint.HasPermission, bevolkingsonderzoekScopes = { Bevolkingsonderzoek.CERVIX }, recht = {
	Recht.GEBRUIKER_CERVIX_LABFORMULIEREN_CONTROLEREN })
public class CervixLabformulierControlerenPage extends CervixLabformulierBasePage
{

	private static final long serialVersionUID = 1L;

	public CervixLabformulierControlerenPage(List<Long> labformulierenIds, CervixLabformulier labformulier)
	{
		add((new CervixLabformulierControlerenPanel("controlerenFormulierPanel", labformulierenIds, labformulier)));
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<>();
		contextMenuItems.add(new GebruikerMenuItem("menu.cervixscreening.labformulieren-controleren", CervixLabformulierenControlerenPage.class));
		contextMenuItems.add(new GebruikerMenuItem("menu.cervixscreening.labformulier-controleren", false, CervixLabformulierControlerenPage.class));
		return contextMenuItems;
	}
}
