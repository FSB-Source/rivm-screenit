
package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie;

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

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.util.AdresUtil;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;

public class OrganisatiePaspoortPanel extends GenericPanel<Instelling>
{

	private static final long serialVersionUID = 1L;

	public OrganisatiePaspoortPanel(String id, final IModel<Instelling> model)
	{
		super(id, new CompoundPropertyModel<Instelling>(model));
		add(new Label("naam"));
		add(new Label("organisatieType"));

		add(new Label("bvos", new IModel<String>()
		{
			private static final long serialVersionUID = 1L;

			@Override
			public String getObject()
			{
				return Bevolkingsonderzoek.getAfkortingen(model.getObject().getOrganisatieType().getBevolkingsonderzoeken());
			}
		}));

		add(new Label("adres", new IModel<String>()
		{
			private static final long serialVersionUID = 1L;

			@Override
			public String getObject()
			{
				if (CollectionUtils.isNotEmpty(model.getObject().getAdressen()))
				{
					return AdresUtil.getAdres(model.getObject().getAdressen().get(0));
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
				if (CollectionUtils.isNotEmpty(model.getObject().getAdressen()))
				{
					return StringUtils.defaultIfBlank(model.getObject().getAdressen().get(0).getPostcode(), "") + "  "
						+ StringUtils.defaultIfBlank(model.getObject().getAdressen().get(0).getPlaats(), "");
				}
				return "";
			}
		}));
	}
}
