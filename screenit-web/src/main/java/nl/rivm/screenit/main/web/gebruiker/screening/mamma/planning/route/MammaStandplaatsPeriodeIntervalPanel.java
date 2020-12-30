package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.route;

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

import java.math.BigDecimal;

import nl.rivm.screenit.dto.mamma.planning.PlanningStandplaatsPeriodeDto;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.dashboard.MammaPlanningDashboardIntervalPanel;
import nl.rivm.screenit.util.BigDecimalUtil;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;

public abstract class MammaStandplaatsPeriodeIntervalPanel extends GenericPanel<PlanningStandplaatsPeriodeDto>
{

	private static final long serialVersionUID = 1L;

	public MammaStandplaatsPeriodeIntervalPanel(String id, IModel<PlanningStandplaatsPeriodeDto> model)
	{
		super(id, model);
		PlanningStandplaatsPeriodeDto standplaatsPeriodeDto = getModelObject();

		BigDecimal initieelInterval = BigDecimalUtil.roundToNearestHalf(standplaatsPeriodeDto.initieelIntervalMaanden);
		add(MammaPlanningDashboardIntervalPanel.intervalRood(initieelInterval, new Label("initieelInterval", initieelInterval)));

		BigDecimal interval = BigDecimalUtil.roundToNearestHalf(standplaatsPeriodeDto.intervalMaanden);
		add(MammaPlanningDashboardIntervalPanel.intervalRood(interval, new Label("interval", interval)));
	}
}
