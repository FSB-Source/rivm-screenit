package nl.rivm.screenit.main.web.gebruiker.algemeen.tooltip;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.service.ClientTooltipService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.model.ClientTooltip;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.ScopeService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.StringValidator;

public class ClientTooltipEditPanel extends GenericPanel<ClientTooltip>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private ClientTooltipService tooltipService;

	@SpringBean
	private ScopeService scopeService;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	public ClientTooltipEditPanel(String id, IModel<ClientTooltip> model)
	{
		super(id, model);
		ToegangLevel toeganglevel = ScreenitSession.get().getToegangsLevel(Actie.AANPASSEN, Recht.CLIENT_TOOLTIP_BEHEER);
		ScreenitForm<ClientTooltip> form = new ScreenitForm<ClientTooltip>("typeForm", getModel());
		form.add(new TextField<String>("titel").add(new StringValidator(1, 255)).setRequired(true));
		form.add(new TextArea<String>("tekst").add(new StringValidator(1, 255)).setRequired(true));
		form.add(new IndicatingAjaxSubmitLink("opslaan", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				ClientTooltip tooltip = (ClientTooltip) form.getModelObject();
				tooltip.setAangepast(dateSupplier.getDate());
				tooltipService.saveOrUpdate(tooltip);
				info("Client Tooltip is opgeslagen");
			}

			@Override
			public boolean isVisible()
			{
				ToegangLevel toeganglevel = ScreenitSession.get().getToegangsLevel(Actie.AANPASSEN, Recht.CLIENT_TOOLTIP_BEHEER);
				return toeganglevel != null && ToegangLevel.LANDELIJK.getNiveau() <= toeganglevel.getNiveau();
			}
		});
		form.setEnabled(toeganglevel != null && ToegangLevel.LANDELIJK.getNiveau() <= toeganglevel.getNiveau());
		add(form);
	}
}
