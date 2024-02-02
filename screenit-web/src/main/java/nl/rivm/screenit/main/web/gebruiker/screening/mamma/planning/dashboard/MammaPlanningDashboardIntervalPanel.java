package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.dashboard;

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

import java.math.BigDecimal;

import nl.rivm.screenit.dto.mamma.planning.PlanningScreeningsEenheidMetaDataDto;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.util.BigDecimalUtil;

import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;

public class MammaPlanningDashboardIntervalPanel extends GenericPanel<MammaScreeningsEenheid>
{

	private static final long serialVersionUID = 1L;

	public MammaPlanningDashboardIntervalPanel(String id, IModel<MammaScreeningsEenheid> model)
	{
		super(id, model);
		PlanningScreeningsEenheidMetaDataDto metaDataDto = model.getObject().getMetaDataDto();

		BigDecimal initieelInterval = BigDecimalUtil.roundToNearestHalf(metaDataDto.initieelIntervalMaanden);
		add(intervalRood(initieelInterval, new Label("initieelInterval", initieelInterval)));

		BigDecimal interval = BigDecimalUtil.roundToNearestHalf(metaDataDto.intervalMaanden);
		add(intervalRood(initieelInterval, new Label("interval", interval)));
	}

	public static Label intervalRood(BigDecimal interval, Label label)
	{
		if (interval != null && interval.subtract(new BigDecimal(24)).abs().compareTo(new BigDecimal(2)) >= 0)
		{
			label.add(new AttributeAppender("class", "text-red"));
		}

		return label;
	}
}
