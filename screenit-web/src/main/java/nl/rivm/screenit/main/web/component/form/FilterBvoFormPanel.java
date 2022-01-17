
package nl.rivm.screenit.main.web.component.form;

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

import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.model.IBevolkingsonderzoek;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;

public abstract class FilterBvoFormPanel<T extends IBevolkingsonderzoek> extends GenericPanel<T>
{

	private static final long serialVersionUID = 1L;

	public FilterBvoFormPanel(String id, final IModel<T> model)
	{
		this(id, model, false, false);
	}

	public FilterBvoFormPanel(String id, IModel<T> model, boolean altijdZichtbaar)
	{
		this(id, model, altijdZichtbaar, false);
	}

	public FilterBvoFormPanel(String id, IModel<T> model, boolean altijdZichtbaar, boolean showExactMatch)
	{
		super(id, new CompoundPropertyModel<>(model));

		Form<T> form = new ScreenitForm<>("bvoFilterForm");
		add(form);
		form.add(new FilterBvoPanel<T>("bvoFilter", model, altijdZichtbaar));

		form.add(new CheckBox("exactMatch").setVisible(showExactMatch));
		form.add(new IndicatingAjaxSubmitLink("filteren", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				IModel<T> filterModel = FilterBvoFormPanel.this.getModel();
				FilterBvoFormPanel.this.doFilter(filterModel, target);
			}
		});

	}

	protected abstract void doFilter(IModel<T> filterModel, AjaxRequestTarget target);
}
