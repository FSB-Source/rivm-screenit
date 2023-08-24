package nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.mamma;

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

import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaBeoordelingGebeurtenis;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.AbstractGebeurtenisDetailPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;

import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.model.IModel;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_CLIENT_MAMMA_HERBEOORDELEN,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaHerbeoordelingGebeurtenisPanel extends AbstractGebeurtenisDetailPanel
{
	public MammaHerbeoordelingGebeurtenisPanel(String id, IModel<ScreeningRondeGebeurtenis> model)
	{
		super(id, model);

		MammaBeoordelingGebeurtenis gebeurtenis = (MammaBeoordelingGebeurtenis) model.getObject();
		boolean redenIngevoerd = gebeurtenis.getMammaBeoordeling().getRedenAnnuleren() != null;

		WebMarkupContainer container = new WebMarkupContainer("container", model);
		container.setVisible(redenIngevoerd);
		add(container);
		container.add(new Label("mammaBeoordeling.redenAnnuleren"));
	}
}
