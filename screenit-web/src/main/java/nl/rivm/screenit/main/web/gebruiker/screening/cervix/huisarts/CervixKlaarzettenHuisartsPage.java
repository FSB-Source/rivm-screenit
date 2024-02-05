package nl.rivm.screenit.main.web.gebruiker.screening.cervix.huisarts;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.web.gebruiker.screening.cervix.CervixScreeningBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Panel;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(constraint = ShiroConstraint.HasPermission, bevolkingsonderzoekScopes = { Bevolkingsonderzoek.CERVIX }, recht = { Recht.GEBRUIKER_KLAARZETTEN_CERVIX_HUISARTS})
public class CervixKlaarzettenHuisartsPage extends CervixScreeningBasePage
{

	private static final long serialVersionUID = 1L;

	private CervixHuisartsOpvraagPanel huisartsZoekPanel;

	private Panel cervixRegistratieUitstrijkendArtsPanel;

	public CervixKlaarzettenHuisartsPage()
	{
		addHuisartsOpvraagPanel();
		cervixRegistratieUitstrijkendArtsPanel = new EmptyPanel("registratieUitstrijkendArtsPanel");
		cervixRegistratieUitstrijkendArtsPanel.setOutputMarkupId(true);
		add(cervixRegistratieUitstrijkendArtsPanel);
	}

	private void addHuisartsOpvraagPanel()
	{
		huisartsZoekPanel = new CervixHuisartsOpvraagPanel("huisartsOpvraagPanel")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void setUitstrijkendArts(AjaxRequestTarget target, CervixHuisarts arts)
			{
				addOrReplaceRegistrerenHuisartsPanel(target, arts);
			}
		};
		add(huisartsZoekPanel);
	}

	private void addOrReplaceRegistrerenHuisartsPanel(AjaxRequestTarget target, CervixHuisarts arts)
	{
		Panel nieuwPanel = new CervixKlaarzettenHuisartsPanel("registratieUitstrijkendArtsPanel", arts)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void closeAanvraag(AjaxRequestTarget target)
			{
				Panel nieuwPanel = new EmptyPanel(cervixRegistratieUitstrijkendArtsPanel.getId());
				cervixRegistratieUitstrijkendArtsPanel.replaceWith(nieuwPanel);
				cervixRegistratieUitstrijkendArtsPanel = nieuwPanel;
				cervixRegistratieUitstrijkendArtsPanel.setOutputMarkupId(true);
				target.add(cervixRegistratieUitstrijkendArtsPanel);
			}

		};
		cervixRegistratieUitstrijkendArtsPanel.replaceWith(nieuwPanel);
		cervixRegistratieUitstrijkendArtsPanel = nieuwPanel;
		cervixRegistratieUitstrijkendArtsPanel.setOutputMarkupId(true);
		target.add(cervixRegistratieUitstrijkendArtsPanel);
	}

}
