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

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.model.IModel;
import org.joda.time.DateTime;
import org.joda.time.MutableDateTime;

public abstract class AjaxDateTimeField extends DateTimeField
{

	public AjaxDateTimeField(String id, IModel<Date> model)
	{
		super(id, model);
	}

	public AjaxDateTimeField(String id)
	{
		super(id);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		getDatePicker().setOutputMarkupId(true);
		getDatePicker().add(new AjaxFormComponentUpdatingBehavior("change")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				AjaxDateTimeField.this.convertInput();
				AjaxDateTimeField.this.updateModel();
				AjaxDateTimeField.this.onUpdate(target);
			}
		});
	}

	@Override
	protected void onConfigure()
	{
		super.onConfigure();

		getDatePicker().setRequired(isRequired());
		getTimeField().setRequired(isRequired());
	}

	@Override
	protected TimeField newTimeField(String wicketId, IModel<Date> model)
	{
		return new AjaxTimeField(wicketId, model)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				AjaxDateTimeField.this.convertInput();
				AjaxDateTimeField.this.updateModel();
				AjaxDateTimeField.this.onUpdate(target);
			}
		};
	}

	@Override
	public void convertInput()
	{
		if ((getDatePicker().getConvertedInput() == null && getDatePicker().getModelObject() == null)
			|| (getTimeField().getConvertedInput() == null && getTimeField().getConvertedInput() == null))
		{
			invalid();
		}
		else
		{

			MutableDateTime datum = null;
			if (getDatePicker().getConvertedInput() != null)
			{
				datum = new MutableDateTime(getDatePicker().getConvertedInput());
			}
			else
			{
				datum = new MutableDateTime(getDatePicker().getModelObject());
			}
			datum.setSecondOfDay(0); 

			DateTime tijd = null;
			if (getTimeField().getConvertedInput() != null)
			{
				tijd = new DateTime(getTimeField().getConvertedInput());
			}
			else
			{
				tijd = new DateTime(getTimeField().getModelObject());
			}

			datum.setHourOfDay(tijd.getHourOfDay());
			datum.setMinuteOfHour(tijd.getMinuteOfHour());

			setConvertedInput(datum.toDate());
		}
	}

	protected abstract void onUpdate(AjaxRequestTarget target);
}
