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
import nl.rivm.screenit.main.web.component.PollingAbstractAjaxTimerBehavior;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.dashboard.MammaPlanningDashboardPage;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.enums.MammaPlanningStatus;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.time.Duration;

public class MammaPlanningStatusPanel extends GenericPanel<PlanningStatusDto>
{

	@SpringBean
	private MammaBaseConceptPlanningsApplicatie conceptPlanningsApplicatie;

	public MammaPlanningStatusPanel(String id, PlanningStatusDto statusDtoIModel)
	{
		super(id, new CompoundPropertyModel<>(statusDtoIModel));
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		WebMarkupContainer container = new WebMarkupContainer("container");
		container.setOutputMarkupId(true);

		container.add(new EnumLabel<MammaPlanningStatus>("status"));
		container.add(new Label("soNaam", new IModel<String>()
		{
			private Long soId = null;

			private String screeningorganisatieNaam = null;

			@Override
			public String getObject()
			{
				if (soId != getModelObject().getSoId())
				{
					screeningorganisatieNaam = getScreeningorganisatieNaam();
					soId = getModelObject().getSoId();
				}
				return screeningorganisatieNaam;
			}
		})
		{
			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(StringUtils.isNotBlank(getDefaultModelObjectAsString()));
			}
		});
		Component terugNaarPlanning = new IndicatingAjaxLink<>("terugNaarPlanning")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(MammaPlanningDashboardPage.class);
			}

			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(MammaPlanningStatusPanel.this.getModelObject().getStatus() == MammaPlanningStatus.OPERATIONEEL);
			}
		};
		container.add(terugNaarPlanning);
		add(container);

		add(new PollingAbstractAjaxTimerBehavior(Duration.seconds(3))
		{
			@Override
			protected void onTimer(AjaxRequestTarget target)
			{
				super.onTimer(target);
				getModel().setObject(conceptPlanningsApplicatie.getStatus());
				target.add(container);
				if (getModelObject().getStatus() == MammaPlanningStatus.OPERATIONEEL)
				{
					onPlanningOperationeel(target);
				}
			}
		});
	}

	protected void onPlanningOperationeel(AjaxRequestTarget target)
	{

	}

	private String getScreeningorganisatieNaam()
	{
		String soNaam = null;
		PlanningStatusDto statusDto = getModelObject();
		if (statusDto.getSoId() != null)
		{
			soNaam = new SimpleHibernateModel<ScreeningOrganisatie>(ScreeningOrganisatie.class, statusDto.getSoId()).getObject().getNaam();
		}
		return soNaam;
	}
}
