package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.beelden;

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

import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractBEAccordionPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaMBBBeoordelingPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaOnderzoekPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaVisueleInspectiePanel;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.util.DateUtil;

import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public class MammaBeeldenInzienOnderzoekPanel extends AbstractBEAccordionPanel<MammaOnderzoek>
{

	public MammaBeeldenInzienOnderzoekPanel(String id, IModel<MammaOnderzoek> model)
	{
		super(id, model, 12);
		super.setTitle(Model.of(Integer.toString(DateUtil.toLocalDate(model.getObject().getCreatieDatum()).getYear())));
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		renderPanelComponents();
	}

	private void renderPanelComponents()
	{
		createVisueleInspectiePanel(panelContainer);
		createOnderzoekPanel(panelContainer);
		createMBBerPanel(panelContainer);
	}

	private void createVisueleInspectiePanel(WebMarkupContainer panelContainer)
	{
		panelContainer.add(new MammaVisueleInspectiePanel("visueleInspectiePanel", getModel()));
	}

	private void createOnderzoekPanel(WebMarkupContainer panelContainer)
	{
		panelContainer.add(new MammaOnderzoekPanel("onderzoekPanel", getModel()));
	}

	private void createMBBerPanel(WebMarkupContainer panelContainer)
	{
		panelContainer.addOrReplace(new MammaMBBBeoordelingPanel("mbberBevindingenPanel", new CompoundPropertyModel<>(getModel()), true));
	}
}
