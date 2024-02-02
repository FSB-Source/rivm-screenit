package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning;

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

import nl.rivm.screenit.dto.mamma.planning.PlanningStatusDto;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.panel.GenericPanel;

public abstract class MammaPlanningStatusPopupPanel extends GenericPanel<PlanningStatusDto>
{

	private final PlanningStatusDto dto;

	public MammaPlanningStatusPopupPanel(String id, PlanningStatusDto dto)
	{
		super(id);
		this.dto = dto;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		add(new MammaPlanningStatusPanel("statusPanel", dto)
		{
			@Override
			protected void onPlanningOperationeel(AjaxRequestTarget target)
			{
				MammaPlanningStatusPopupPanel.this.onPlanningOperationeel(target);
			}
		});

		add(new IndicatingAjaxLink<>("verlaten")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(MammaPlanningNietOperationeelPage.class);
			}
		});
	}

	protected abstract void onPlanningOperationeel(AjaxRequestTarget target);
}
