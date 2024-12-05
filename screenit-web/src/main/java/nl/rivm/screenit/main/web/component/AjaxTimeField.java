package nl.rivm.screenit.main.web.component;

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

import java.util.Date;

import org.apache.wicket.ajax.AbstractDefaultAjaxBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes.Method;
import org.apache.wicket.ajax.attributes.CallbackParameter;
import org.apache.wicket.markup.ComponentTag;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.model.IModel;
import org.wicketstuff.wiquery.core.javascript.JsQuery;
import org.wicketstuff.wiquery.core.javascript.JsScope;
import org.wicketstuff.wiquery.core.javascript.JsStatement;
import org.wicketstuff.wiquery.core.javascript.JsUtils;

public abstract class AjaxTimeField extends TimeField
{

	private static final long serialVersionUID = 1L;

	private TimeFieldBehavior behavior;

	public AjaxTimeField(String id, IModel<Date> model)
	{
		super(id, model);
	}

	public AjaxTimeField(String id)
	{
		super(id);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		getHoursField().setOutputMarkupId(true);
		getMinutesField().setOutputMarkupId(true);

		this.behavior = new TimeFieldBehavior();
		add(behavior);
	}

	@Override
	public void renderHead(IHeaderResponse response)
	{
		response.render(JavaScriptHeaderItem.forReference(TimerResourceReference.get()));

		String selector = String.format("#%s, #%s", getHoursField().getMarkupId(), getMinutesField()
			.getMarkupId());

		String label = JsUtils.doubleQuotes("trigger");

		JsStatement stopTime = new JsQuery(getHoursField()).$().chain("stopTime", label);

		CharSequence function = behavior.getCallbackFunction(
			CallbackParameter.resolved(getHoursField().getInputName(), new JsQuery(
				getHoursField()).$().chain("val").render(false).toString()),
			CallbackParameter.resolved(getMinutesField().getInputName(), new JsQuery(getMinutesField()).$()
				.chain("val").render(false).toString()));

		JsStatement oneTime = new JsQuery(getHoursField()).$()
			.chain("oneTime", "500", label, function);

		JsStatement query = new JsQuery().$(selector).chain(
			"keyup", JsScope.quickScope(String.format("%s%s", stopTime.render(), oneTime.render())).render());

		response.render(OnDomReadyHeaderItem.forScript(query.render()));
	}

	protected abstract void onUpdate(AjaxRequestTarget target);

	private class TimeFieldBehavior extends AbstractDefaultAjaxBehavior
	{

		private static final long serialVersionUID = 1L;

		@Override
		protected void respond(AjaxRequestTarget target)
		{
			updateComponent(getHoursField());
			updateComponent(getMinutesField());

			if (getHoursField().isValid() && getMinutesField().isValid())
			{
				convertInput();
				updateModel();
				onUpdate(target);
			}
		}

		protected void updateComponent(FormComponent<?> formComponent)
		{
			formComponent.inputChanged();
			formComponent.validate();
			if (formComponent.hasErrorMessage())
			{
				formComponent.invalid();
			}
			else
			{
				formComponent.valid();
				formComponent.updateModel();
			}
		}

		@Override
		protected void updateAjaxAttributes(AjaxRequestAttributes attributes)
		{
			super.updateAjaxAttributes(attributes);

			attributes.setMethod(Method.POST);
		}
	}

	@Override
	protected void onComponentTag(ComponentTag tag)
	{
		if ("wicket:container".equals(tag.getName()))
		{
			throw new IllegalArgumentException("Ajax timefield kan niet aan een wicket:container worden gehangen");
		}

		super.onComponentTag(tag);
	}
}
