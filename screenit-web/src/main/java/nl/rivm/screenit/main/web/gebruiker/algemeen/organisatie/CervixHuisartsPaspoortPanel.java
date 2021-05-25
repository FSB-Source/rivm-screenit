
package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie;

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

import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.util.AdresUtil;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;

public class CervixHuisartsPaspoortPanel extends GenericPanel<CervixHuisarts>
{

	private static final long serialVersionUID = 1L;

	public CervixHuisartsPaspoortPanel(String id, final IModel<CervixHuisarts> model)
	{
		super(id, new CompoundPropertyModel<CervixHuisarts>(model));
		add(new Label("naam"));
		add(new Label("organisatieType"));
		add(new Label("bvos", Bevolkingsonderzoek.CERVIX.getAfkorting()));
		add(new Label("adres", new IModel<String>()
		{
			private static final long serialVersionUID = 1L;

			@Override
			public String getObject()
			{
				if (model.getObject().getPostadres() != null)
				{
					return AdresUtil.getAdres(model.getObject().getPostadres());
				}
				return "";
			}
		}));
		add(new Label("postcodePlaats", new IModel<String>()
		{
			private static final long serialVersionUID = 1L;

			@Override
			public String getObject()
			{
				if (model.getObject().getPostadres() != null)
				{
					return StringUtils.defaultIfBlank(model.getObject().getPostadres().getPostcode(), "") + "  "
						+ StringUtils.defaultIfBlank(model.getObject().getPostadres().getWoonplaats().getNaam(), "");
				}
				return "";
			}
		}));
		add(new Label("agbcode"));
	}
}
