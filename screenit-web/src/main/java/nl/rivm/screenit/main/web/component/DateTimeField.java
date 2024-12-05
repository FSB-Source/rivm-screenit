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

import org.apache.wicket.markup.html.form.FormComponentPanel;
import org.apache.wicket.model.IModel;
import org.joda.time.DateTime;
import org.joda.time.MutableDateTime;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

public class DateTimeField extends FormComponentPanel<Date>
{

	private static final long serialVersionUID = 1L;

	private DatePicker<Date> datePicker;

	private TimeField timeField;

	public DateTimeField(String id)
	{
		this(id, null);
	}

	public DateTimeField(String id, IModel<Date> model)
	{
		super(id, model);

		setType(Date.class);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		datePicker = newDatePicker("datePicker", getModel());
		add(datePicker);
		timeField = newTimeField("timeField", getModel());
		add(timeField);
	}

	protected DatePicker<Date> newDatePicker(String wicketId, IModel<Date> model)
	{
		return DatePickerHelper.newDatePicker(wicketId, model);
	}

	protected TimeField newTimeField(String wicketId, IModel<Date> model)
	{
		return new TimeField(wicketId, model);
	}

	@Override
	public void convertInput()
	{
		if (datePicker.getConvertedInput() == null || timeField.getConvertedInput() == null)
		{
			invalid();
		}
		else
		{

			MutableDateTime datum = new MutableDateTime(datePicker.getConvertedInput());
			datum.setSecondOfDay(0); 

			DateTime tijd = new DateTime(timeField.getConvertedInput());

			datum.setHourOfDay(tijd.getHourOfDay());
			datum.setMinuteOfHour(tijd.getMinuteOfHour());

			setConvertedInput(datum.toDate());
		}
	}

	protected DatePicker<Date> getDatePicker()
	{
		return datePicker;
	}

	protected TimeField getTimeField()
	{
		return timeField;
	}
}
