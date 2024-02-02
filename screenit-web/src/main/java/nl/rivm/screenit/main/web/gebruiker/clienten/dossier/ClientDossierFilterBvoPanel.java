
package nl.rivm.screenit.main.web.gebruiker.clienten.dossier;

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

import nl.rivm.screenit.main.service.ClientDossierFilter;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.form.FilterBvoPanel;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.poi.ss.formula.functions.T;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;

public abstract class ClientDossierFilterBvoPanel extends GenericPanel<ClientDossierFilter>
{

	private static final long serialVersionUID = 1L;

	public ClientDossierFilterBvoPanel(String id, IModel<ClientDossierFilter> model)
	{
		super(id, model);

		Form<T> form = new ScreenitForm<>("bvoFilterForm");
		add(form);
		form.add(new FilterBvoPanel<ClientDossierFilter>("bvoFilter", model, true));
		form.add(ComponentHelper.newCheckBox("laatsteRondes", new PropertyModel<Boolean>(model, "laatsteRondes")));
		form.add(new IndicatingAjaxSubmitLink("filteren", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				IModel<ClientDossierFilter> filterModel = ClientDossierFilterBvoPanel.this.getModel();
				ScreenitSession.get().setZoekObject(ClientDossierFilterBvoPanel.class, filterModel);
				ClientDossierFilterBvoPanel.this.doFilter(filterModel, target);
			}
		});
	}

	protected abstract void doFilter(IModel<ClientDossierFilter> filterModel, AjaxRequestTarget target);
}
