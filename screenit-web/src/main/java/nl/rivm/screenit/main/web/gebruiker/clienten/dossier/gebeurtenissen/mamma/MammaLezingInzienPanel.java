package nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.mamma;

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

import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaLezingGebeurtenis;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.AbstractGebeurtenisDetailPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaReadOnlyLezingPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;

import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.model.IModel;
import org.wicketstuff.shiro.ShiroConstraint;
import org.wicketstuff.wiquery.core.javascript.JsStatement;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_MAMMA_INZIEN_LEZING,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaLezingInzienPanel extends AbstractGebeurtenisDetailPanel
{
	public MammaLezingInzienPanel(String id, IModel<ScreeningRondeGebeurtenis> model)
	{
		super(id, model);
		MammaLezingGebeurtenis gebeurtenis = (MammaLezingGebeurtenis) model.getObject();
		add(new MammaReadOnlyLezingPanel("mammaReadOnlyLezingPanel", gebeurtenis.getMammaBeoordeling(), gebeurtenis.getLezing(), true, true));
	}

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);

		JsStatement jsStatement = new JsStatement();
		jsStatement.append(String.format("new MutationObserver(calcLaesiePositions).observe($('#%1$s')[0], { attributes: true })", getParent().getParent().getParent().getMarkupId()));
		response.render(OnDomReadyHeaderItem.forScript(jsStatement.render()));
	}
}
