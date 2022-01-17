package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be;

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

import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.enums.MammaFollowUpConclusieStatus;

import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public class MammaBeFollowUpConclusiePanel extends AbstractBEAccordionPanel<MammaBeoordeling>
{
	public MammaBeFollowUpConclusiePanel(String id, IModel<MammaBeoordeling> model)
	{
		super(id, model, Model.of("Follow Up Conclusie"), 6);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		MammaFollowUpConclusieStatus followUpConclusieStatus = getModel().getObject().getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde()
			.getFollowUpConclusieStatus();

		panelContainer.add(new EnumLabel<MammaFollowUpConclusieStatus>("followUpConclusie", followUpConclusieStatus)
		{
			@Override
			protected String nullValue()
			{
				return "Geen";
			}
		});
	}
}
