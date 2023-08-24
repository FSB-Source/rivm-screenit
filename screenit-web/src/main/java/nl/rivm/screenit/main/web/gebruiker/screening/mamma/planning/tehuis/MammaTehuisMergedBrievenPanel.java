
package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.tehuis;

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

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;

public class MammaTehuisMergedBrievenPanel extends Panel
{
	private static final long serialVersionUID = 1L;

	public MammaTehuisMergedBrievenPanel(String id, int aantalClientMetProjectBrief, int aantalClientMetBrief, int aantalClientMetSuspectBrief)
	{
		super(id);
		add(new Label("aantalClientMetBrief", aantalClientMetBrief).setVisible(aantalClientMetBrief > 0));
		add(new Label("aantalClientMetProjectBrief", aantalClientMetProjectBrief).setVisible(aantalClientMetProjectBrief > 0));
		add(new Label("aantalClientMetSuspectBrief", aantalClientMetSuspectBrief));
	}

}
