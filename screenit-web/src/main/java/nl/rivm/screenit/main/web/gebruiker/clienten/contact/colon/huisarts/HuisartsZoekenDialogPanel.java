package nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon.huisarts;

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

import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.ScreeningRonde;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.panel.GenericPanel;

public abstract class HuisartsZoekenDialogPanel extends GenericPanel<ScreeningRonde>
{

	public HuisartsZoekenDialogPanel(String id)
	{
		super(id);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		add(new HuisartsZoekenPanel("huisartsZoeken", false)
		{
			@Override
			protected void onHuisartsGekozen(AjaxRequestTarget target, EnovationHuisarts huisarts)
			{
				HuisartsZoekenDialogPanel.this.onHuisartsGekozen(target, huisarts);
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
	}

	protected abstract void close(AjaxRequestTarget target);

	protected abstract void onHuisartsGekozen(AjaxRequestTarget target, EnovationHuisarts huisarts);

}
