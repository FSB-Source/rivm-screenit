package nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.AbstractGebeurtenisDetailPanel;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.OnbekendeHuisarts;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.NaamUtil;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public class ColonHuisartsInzienPanel extends AbstractGebeurtenisDetailPanel
{
	public ColonHuisartsInzienPanel(String id, IModel<ScreeningRondeGebeurtenis> model)
	{
		super(id, model);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		ColonScreeningRonde ronde = getModelObject().getScreeningsRonde();
		EnovationHuisarts enovationHuisarts = ronde.getColonHuisarts();
		OnbekendeHuisarts onbekendeHuisarts = ronde.getOnbekendeHuisarts();

		if (enovationHuisarts != null)
		{
			add(new Label("huisartsNaam", NaamUtil.getNaamHuisarts(enovationHuisarts)));
			add(new Label("praktijkNaam", enovationHuisarts.getPraktijknaam()));
			add(new Label("praktijkAdres", AdresUtil.getVolledigeAdresString(enovationHuisarts.getAdres())));
			String agbCode = enovationHuisarts.getHuisartsAgb();
			if (StringUtils.isBlank(agbCode))
			{
				agbCode = enovationHuisarts.getPraktijkAgb();
			}
			add(new Label("huisartsAgb", agbCode));
			add(new Label("weergaveNaam", enovationHuisarts.getWeergavenaam()));
			add(new Label("klantnummer", enovationHuisarts.getKlantnummer()));
			add(new Label("ediadres", enovationHuisarts.getOorspronkelijkEdiadres()));
			add(new Label("communicatieadres", enovationHuisarts.getEdiadres()));
		}
		else if (onbekendeHuisarts != null)
		{
			add(new Label("huisartsNaam", onbekendeHuisarts.getHuisartsNaam()));
			add(new Label("praktijkNaam", onbekendeHuisarts.getPraktijkNaam()));
			add(new Label("praktijkAdres", AdresUtil.getOnbekendeHuisartsAdres(onbekendeHuisarts)));
			add(new Label("huisartsAgb", Model.of("")));
			add(new Label("weergaveNaam", Model.of("")));
			add(new Label("klantnummer", Model.of("")));
			add(new Label("ediadres", Model.of("")));
			add(new Label("communicatieadres", Model.of("")));
		}
		else
		{
			add(new Label("huisartsNaam", Model.of("")));
			add(new Label("praktijkNaam", Model.of("")));
			add(new Label("praktijkAdres", Model.of("")));
			add(new Label("huisartsAgb", Model.of("")));
			add(new Label("weergaveNaam", Model.of("")));
			add(new Label("klantnummer", Model.of("")));
			add(new Label("ediadres", Model.of("")));
			add(new Label("communicatieadres", Model.of("")));
		}
	}

}
