package nl.rivm.screenit.main.web.component;

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

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.IAjaxIndicatorAware;
import org.apache.wicket.ajax.form.AjaxFormChoiceComponentUpdatingBehavior;
import org.apache.wicket.behavior.Behavior;
import org.apache.wicket.extensions.ajax.markup.html.AjaxIndicatorAppender;
import org.apache.wicket.markup.html.form.FormComponent;

public abstract class IndicatingAjaxFormChoiceComponentUpdatingBehavior extends AjaxFormChoiceComponentUpdatingBehavior implements IAjaxIndicatorAware
{
	private final AjaxIndicatorAppender indicatorAppender = new AjaxIndicatorAppender();

	protected IndicatingAjaxFormChoiceComponentUpdatingBehavior(FormComponent<?> owner)
	{
		super();
		owner.add(new Behavior[] { this.indicatorAppender });
	}

	protected void onUpdate(AjaxRequestTarget target)
	{
		StringBuilder sb = new StringBuilder();
		sb.append("$('#showLink').click();");
		target.prependJavaScript(sb.toString());
		this.onComponentUpdate(target);
	}

	protected abstract void onComponentUpdate(AjaxRequestTarget var1);

	public String getAjaxIndicatorMarkupId()
	{
		return this.indicatorAppender.getMarkupId();
	}
}
