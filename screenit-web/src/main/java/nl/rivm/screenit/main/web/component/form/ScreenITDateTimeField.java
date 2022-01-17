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

import java.util.Date;

import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.topicuszorg.wicket.planning.web.component.AjaxDateTimeField;

import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.markup.head.OnLoadHeaderItem;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.joda.time.DateTime;
import org.joda.time.MutableDateTime;
import org.wicketstuff.wiquery.core.javascript.JsQuery;
import org.wicketstuff.wiquery.ui.JQueryUIJavaScriptResourceReference;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;
import org.wicketstuff.wiquery.ui.datepicker.DatePickerLanguageResourceReference;

public abstract class ScreenITDateTimeField extends AjaxDateTimeField
{

	private static final long serialVersionUID = 1L;

	private boolean magDatumWijzigen = true;

	public ScreenITDateTimeField(String id)
	{
		super(id);
	}

	public ScreenITDateTimeField(String id, boolean magDatumWijzigen)
	{
		super(id);
		this.magDatumWijzigen = magDatumWijzigen;
	}

	public abstract String getDatePickerLabel();

	@Override
	protected DatePicker<Date> newDatePicker(String wicketId, IModel<Date> model)
	{
		DatePicker<Date> datePicker = new DatePicker<Date>(wicketId, model)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void renderHead(IHeaderResponse response)
			{
				response.render(JavaScriptHeaderItem.forReference(JQueryUIJavaScriptResourceReference.get()));

				DatePickerLanguageResourceReference dpl = DatePickerLanguageResourceReference.get(getLocale());
				if (dpl != null)
				{
					response.render(JavaScriptHeaderItem.forReference(dpl));
				}

				response.render(OnLoadHeaderItem.forScript(new JsQuery(this).$().chain("datepicker", getOptions().getJavaScriptOptions()).render().toString()));
			}

			@Override
			public void updateModel()
			{
				MutableDateTime convertedInput = new MutableDateTime(getConvertedInput());
				MutableDateTime startDateTime = new MutableDateTime(getModelObject());
				convertedInput.setMinuteOfHour(startDateTime.getMinuteOfHour());
				convertedInput.setHourOfDay(startDateTime.getHourOfDay());
				convertedInput.setSecondOfMinute(0); 
				convertedInput.setMillisOfSecond(0);
				setModelObject(convertedInput.toDate());
			}

		};
		datePicker.setDateFormat("dd-mm-yy");
		datePicker.setEnabled(magDatumWijzigen);
		datePicker.setLabel(Model.of(getDatePickerLabel()));
		datePicker.add(ComponentHelper.newDbRangeValidator());
		return datePicker;
	}

	@Override
	public void convertInput()
	{
		if (getDatePicker().getConvertedInput() == null && getDatePicker().getModelObject() == null && getTimeField().getConvertedInput() == null
			&& getTimeField().getModelObject() == null)
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
			datum.setMillisOfSecond(0);

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

}
