package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.route;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.dto.mamma.planning.PlanningStandplaatsPeriodeDto;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;

public abstract class MammaStandplaatsPeriodeBlokkadesPanel extends GenericPanel<PlanningStandplaatsPeriodeDto>
{

	private static final long serialVersionUID = 1L;

	public MammaStandplaatsPeriodeBlokkadesPanel(String id, IModel<PlanningStandplaatsPeriodeDto> model)
	{
		super(id, model);
		PlanningStandplaatsPeriodeDto standplaatsPeriodeDto = getModelObject();

		WebMarkupContainer blokkades = new WebMarkupContainer("blokkades");

		if (CollectionUtils.isNotEmpty(standplaatsPeriodeDto.blokkadeIds))
		{
			addBlokkadeTooltip(blokkades, getModel());
			blokkades.add(new AttributeAppender("class", " icon-red"));
		}

		add(blokkades);

	}

	protected abstract void addBlokkadeTooltip(WebMarkupContainer blokkades, IModel<PlanningStandplaatsPeriodeDto> model);
}
