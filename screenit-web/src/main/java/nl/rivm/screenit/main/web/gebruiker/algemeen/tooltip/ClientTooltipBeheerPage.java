package nl.rivm.screenit.main.web.gebruiker.algemeen.tooltip;

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

import java.util.Arrays;

import nl.rivm.screenit.main.service.ClientTooltipFilter;
import nl.rivm.screenit.main.service.ClientTooltipService;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.gebruiker.algemeen.AlgemeenPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.ClientTooltip;
import nl.rivm.screenit.model.ClientTooltipType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.CLIENT_TOOLTIP_BEHEER,
	level = ToegangLevel.LANDELIJK,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class ClientTooltipBeheerPage extends AlgemeenPage
{
	private final IModel<ClientTooltipFilter> filter;

	private WebMarkupContainer typeContainer;

	private final Label selecteerEenType;

	@SpringBean
	private ClientTooltipService tooltipService;

	public ClientTooltipBeheerPage()
	{
		filter = new Model<>(new ClientTooltipFilter());

		add(new FilterForm("typeForm", filter));
		typeContainer = new WebMarkupContainer("typeContainer");
		typeContainer.setOutputMarkupPlaceholderTag(true);
		add(typeContainer);
		selecteerEenType = new Label("selecteerEenType", getString("selecteerEenType"));
		selecteerEenType.setOutputMarkupPlaceholderTag(true);
		add(selecteerEenType);
	}

	private class FilterForm extends Form<ClientTooltipFilter>
	{
		public FilterForm(String id, IModel<ClientTooltipFilter> model)
		{
			super(id, new CompoundPropertyModel<>(model));

			ScreenitDropdown<ClientTooltipType> typeSelectie = new ScreenitDropdown<>("type", Arrays.asList(ClientTooltipType.values()));
			typeSelectie.setChoiceRenderer(new EnumChoiceRenderer<>(typeSelectie));
			typeSelectie.add(new AjaxFormComponentUpdatingBehavior("change")
			{
				@Override
				protected void onUpdate(AjaxRequestTarget target)
				{
					if (filter.getObject() != null && filter.getObject().getType() != null)
					{
						var tooltip = tooltipService.getClientTooltipByType(filter.getObject().getType());

						if (tooltip == null)
						{
							tooltip = new ClientTooltip();
							tooltip.setType(filter.getObject().getType());
						}

						ClientTooltipEditPanel editPanel = new ClientTooltipEditPanel("typeContainer", ModelUtil.cModel(tooltip));
						editPanel.setOutputMarkupPlaceholderTag(true);
						typeContainer.replaceWith(editPanel);
						typeContainer = editPanel;
						target.add(typeContainer);
						selecteerEenType.setVisible(false);
						target.add(selecteerEenType);
					}
					else
					{
						WebMarkupContainer editPanel = new WebMarkupContainer("typeContainer");
						editPanel.setOutputMarkupPlaceholderTag(true);
						typeContainer.replaceWith(editPanel);
						typeContainer = editPanel;
						selecteerEenType.setVisible(true);
						target.add(selecteerEenType);
						target.add(typeContainer);
					}
				}
			});
			typeSelectie.setNullValid(false);
			typeSelectie.setOutputMarkupId(true);
			add(typeSelectie);
		}
	}
}
