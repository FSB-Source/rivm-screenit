package nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma.huisarts;

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

import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaGeenHuisartsOption;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.NaamUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;

public abstract class MammaHuisartsVorigeRondePopupPanel extends GenericPanel<EnovationHuisarts>
{

	public MammaHuisartsVorigeRondePopupPanel(String id, IModel<EnovationHuisarts> haUitVorigeRondeModel)
	{
		super(id, haUitVorigeRondeModel);

		EnovationHuisarts ha = getModelObject();
		add(new Label("huisartsNaam", NaamUtil.getNaamHuisarts(ha)));
		add(new Label("praktijknaam"));
		add(new Label("praktijkAdres", AdresUtil.getVolledigeAdresString(ha.getAdres())));
		add(new Label("telefoonnummer"));
		add(new Label("huisartsAgb"));
		add(new Label("praktijkAgb"));
		add(new Label("klantnummer"));
		add(new Label("oorspronkelijkEdiadres"));
		add(new Label("ediadres"));
		add(new IndicatingAjaxLink<MammaScreeningRonde>("terug")
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				close(target);
				openZoekHuisartsPopup(target);
			}
		});
		add(new AjaxLink<Void>("annuleren")
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				close(target);
			}
		});
		add(new IndicatingAjaxLink<MammaScreeningRonde>("opslaan")
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				EnovationHuisarts ha = MammaHuisartsVorigeRondePopupPanel.this.getModelObject();
				onHuisartsGekozen(target, ha, null);
			}
		});
	}

	protected abstract void close(AjaxRequestTarget target);

	protected abstract void onHuisartsGekozen(AjaxRequestTarget target, EnovationHuisarts huisarts, MammaGeenHuisartsOption geenHuisartsOptie);

	protected abstract void openZoekHuisartsPopup(AjaxRequestTarget target);

}
