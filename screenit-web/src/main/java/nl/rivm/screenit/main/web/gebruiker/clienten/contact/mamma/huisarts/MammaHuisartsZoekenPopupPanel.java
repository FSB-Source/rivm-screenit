package nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma.huisarts;

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

import java.util.Arrays;

import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon.huisarts.HuisartsZoekenPanel;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.mamma.enums.MammaGeenHuisartsOption;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public abstract class MammaHuisartsZoekenPopupPanel extends GenericPanel<EnovationHuisarts>
{
	private boolean geenHuisartsOptiesBeschikbaar;

	public MammaHuisartsZoekenPopupPanel(String id, boolean terugNaarZoeken, boolean geenHuisartsOptiesBeschikbaar)
	{
		super(id, ModelUtil.ccModel(new EnovationHuisarts()));
		this.geenHuisartsOptiesBeschikbaar = geenHuisartsOptiesBeschikbaar;
		add(new HuisartsZoekenPanel("huisartsZoeken", terugNaarZoeken)
		{
			@Override
			protected void onHuisartsGekozen(AjaxRequestTarget target, EnovationHuisarts huisarts)
			{
				MammaHuisartsZoekenPopupPanel.this.onHuisartsGekozen(target, huisarts, null);

			}
		});
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		IModel<MammaGeenHuisartsOption> optie = Model.of();
		ScreenitDropdown<MammaGeenHuisartsOption> andersOptiesDropDown = ComponentHelper.addDropDownChoice(this, "andersOptie", false,
			Arrays.asList(MammaGeenHuisartsOption.values()), false);
		andersOptiesDropDown.add(new AjaxFormComponentUpdatingBehavior("change")
		{

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				onHuisartsGekozen(target, null, optie.getObject());
			}
		});

		andersOptiesDropDown.setModel(optie);
		andersOptiesDropDown.setVisible(geenHuisartsOptiesBeschikbaar);

		add(new AjaxLink<Void>("annuleren")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				close(target);
			}
		});
	}

	protected abstract void close(AjaxRequestTarget target);

	protected abstract void onHuisartsGekozen(AjaxRequestTarget target, EnovationHuisarts huisarts, MammaGeenHuisartsOption mammaGeenHuisartsOption);
}
