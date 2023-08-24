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
import nl.rivm.screenit.main.model.mamma.onderzoek.MammaOnderzoekGebeurtenis;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.AbstractGebeurtenisDetailPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaMBBBeoordelingPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaOnderzoekPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaVisueleInspectiePanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.model.IModel;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_MAMMA_INZIEN_ONDERZOEK,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaOnderzoekInzienPanel extends AbstractGebeurtenisDetailPanel
{
	public MammaOnderzoekInzienPanel(String id, IModel<ScreeningRondeGebeurtenis> model)
	{
		super(id, model);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		final MammaOnderzoekGebeurtenis onderzoekGebeurtenis = (MammaOnderzoekGebeurtenis) getModelObject();
		final IModel<MammaOnderzoek> onderzoekModel = ModelUtil.csModel(onderzoekGebeurtenis.getOnderzoek());
		add(new EnumLabel<>("onderzoek.mammografie.ilmStatus"));
		add(new MammaVisueleInspectiePanel("visueleInspectiePanel", onderzoekModel));
		add(new MammaOnderzoekPanel("onderzoekPanel", onderzoekModel));
		add(new MammaMBBBeoordelingPanel("mbberBevindingenPanel", onderzoekModel, true));
	}
}
