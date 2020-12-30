package nl.rivm.screenit.main.web.client.dashboard.mamma;

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

import nl.rivm.screenit.model.mamma.enums.MammaGeenHuisartsOption;

import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.panel.Panel;

class GeenHuisartsOptiePanel extends Panel
{
	private static final long serialVersionUID = 1L;

	public GeenHuisartsOptiePanel(String id, MammaGeenHuisartsOption geenHuisartsOption)
	{
		super(id);
		add(new EnumLabel<MammaGeenHuisartsOption>("geenHuisartsOptie", geenHuisartsOption));
	}
}