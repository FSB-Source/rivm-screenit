package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be;

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

import nl.rivm.screenit.model.mamma.MammaBeoordeling;

import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public class MammaHistorischeLezingenPanel extends AbstractBEAccordionPanel<MammaBeoordeling>
{

	public MammaHistorischeLezingenPanel(String id, IModel<MammaBeoordeling> model)
	{
		super(id, model, Model.of("Lezingen"), 12);
		super.setIngeklapt(false);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		final MammaBeoordeling beoordeling = getModelObject();
		boolean nevenbevindingenWeergeven = false;
		boolean redenenFotobesprekingWeergeven = false;
		boolean tweeLezingenIngevoerd = beoordeling.getTweedeLezing() != null;

		if (tweeLezingenIngevoerd)
		{
			panelContainer
					.add(new MammaReadOnlyLezingPanel("eersteLezing", beoordeling, beoordeling.getEersteLezing(), nevenbevindingenWeergeven,
							redenenFotobesprekingWeergeven));
			panelContainer
					.add(new MammaReadOnlyLezingPanel("tweedeLezing", beoordeling, beoordeling.getTweedeLezing(), nevenbevindingenWeergeven,
							redenenFotobesprekingWeergeven));
		}
		else
		{
			panelContainer.add(new EmptyPanel("eersteLezing"));
			panelContainer.add(new EmptyPanel("tweedeLezing"));
		}

		if (beoordeling.getVerslagLezing() != null)
		{
			panelContainer
				.add(new MammaReadOnlyLezingPanel("verslagLezing", beoordeling, beoordeling.getVerslagLezing(), nevenbevindingenWeergeven,
					redenenFotobesprekingWeergeven));
		}
		else
		{
			panelContainer.add(new EmptyPanel("verslagLezing"));
		}

		if (beoordeling.getArbitrageLezing() != null)
		{
			panelContainer
				.add(new MammaReadOnlyLezingPanel("arbitrageOfDiscrepantieLezing", beoordeling, beoordeling.getArbitrageLezing(),
					nevenbevindingenWeergeven, redenenFotobesprekingWeergeven));
		}
		else if (beoordeling.getDiscrepantieLezing() != null)
		{
			panelContainer
				.add(new MammaReadOnlyLezingPanel("arbitrageOfDiscrepantieLezing", beoordeling, beoordeling.getDiscrepantieLezing(),
					nevenbevindingenWeergeven, redenenFotobesprekingWeergeven));
		}
		else
		{
			panelContainer.add(new EmptyPanel("arbitrageOfDiscrepantieLezing"));
		}
	}
}
