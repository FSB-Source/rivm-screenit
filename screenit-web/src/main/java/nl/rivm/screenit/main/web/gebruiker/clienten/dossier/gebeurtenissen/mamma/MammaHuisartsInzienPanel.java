package nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.mamma;

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

import nl.rivm.screenit.main.model.BaseHuisartsModel;
import nl.rivm.screenit.main.model.EnovationHuisartsModel;
import nl.rivm.screenit.main.model.GeenHuisartsModel;
import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.web.gebruiker.algemeen.huisarts.HuisartsInfoPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.AbstractGebeurtenisDetailPanel;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaGeenHuisartsOption;

import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.model.IModel;

public class MammaHuisartsInzienPanel extends AbstractGebeurtenisDetailPanel
{
	public MammaHuisartsInzienPanel(String id, IModel<ScreeningRondeGebeurtenis> model)
	{
		super(id, model);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		MammaScreeningRonde ronde = getModelObject().getScreeningsRonde();
		EnovationHuisarts enovationHuisarts = ronde.getHuisarts();
		MammaGeenHuisartsOption geenHuisartsOptie = ronde.getGeenHuisartsOptie();

		BaseHuisartsModel<?> huisartsModel = null;
		if (enovationHuisarts != null)
		{
			huisartsModel = new EnovationHuisartsModel(enovationHuisarts);
		}
		else
		{
			huisartsModel = new GeenHuisartsModel();
		}
		add(new HuisartsInfoPanel("huisartsInfo", huisartsModel).setVisible(geenHuisartsOptie == null));
		add(new EnumLabel<>("geenHuisartsOptie", geenHuisartsOptie).setVisible(geenHuisartsOptie != null));
	}

}
