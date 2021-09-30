
package nl.rivm.screenit.main.web.gebruiker.screening.colon.gebieden;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.ColonScreeningBasePage;
import nl.rivm.screenit.model.Gemeente;

import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.model.IModel;

public class GebiedenBeheerPage extends ColonScreeningBasePage
{
	private static final long serialVersionUID = 1L;

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<GebruikerMenuItem>();
		contextMenuItems.add(new GebruikerMenuItem("menu.beheer.gemeente.zoeken", GemeenteZoeken.class));
		contextMenuItems.add(new GebruikerMenuItem("menu.beheer.gemeentegegevens", GemeenteGegevens.class)
		{
			private static final long serialVersionUID = 1L;

			@Override
			public IndicatingAjaxLink<?> createWicketLink(String markupId)
			{
				return new IndicatingAjaxLink<Gemeente>(markupId, (IModel) GebiedenBeheerPage.this.getDefaultModel())
				{
					private static final long serialVersionUID = 1L;

					@Override
					public void onClick(AjaxRequestTarget target)
					{
						setResponsePage(new GemeenteGegevens(getModel()));
					}
				};
			}
		});

		return contextMenuItems;
	}

	protected boolean magAdherentieAanpassen()
	{
		return ScreenitSession.get().checkPermission(Recht.GEBRUIKER_BEHEER_GEBIEDEN_ADHERENTIE_AANPASSEN, Actie.AANPASSEN);
	}

}
