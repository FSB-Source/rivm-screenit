package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.route;

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

import nl.rivm.screenit.dto.mamma.planning.PlanningStandplaatsPeriodeDto;

import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;

public abstract class MammaStandplaatsPeriodeIndicatiePanel extends GenericPanel<PlanningStandplaatsPeriodeDto>
{

	private static final long serialVersionUID = 1L;

	public MammaStandplaatsPeriodeIndicatiePanel(String id, IModel<PlanningStandplaatsPeriodeDto> model)
	{
		super(id, model);
		PlanningStandplaatsPeriodeDto standplaatsPeriodeDto = getModelObject();

		WebMarkupContainer meldingen = new WebMarkupContainer("meldingen");
		addMeldingTooltip(meldingen, getModel());

		meldingen.add(new AttributeAppender("class", " " + standplaatsPeriodeDto.meldingenDto.niveau.getCssClass()));
		add(meldingen);
	}

	protected abstract void addMeldingTooltip(WebMarkupContainer meldingen, IModel<PlanningStandplaatsPeriodeDto> iModel);
}
