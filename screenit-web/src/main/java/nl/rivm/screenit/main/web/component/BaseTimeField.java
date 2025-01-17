package nl.rivm.screenit.main.web.component;

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

import java.util.TimeZone;

import nl.topicuszorg.wicket.input.converters.VoorloopNulConverter;

import org.apache.wicket.Application;
import org.apache.wicket.Session;
import org.apache.wicket.core.request.ClientInfo;
import org.apache.wicket.markup.html.form.FormComponentPanel;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.protocol.http.request.WebClientInfo;
import org.apache.wicket.util.convert.IConverter;
import org.apache.wicket.validation.validator.RangeValidator;

public class BaseTimeField<T> extends FormComponentPanel<T>
{

	private static final long serialVersionUID = 1L;

	private static final IConverter<Integer> MINUTES_CONVERTER = VoorloopNulConverter.INSTANCE;

	private Integer hours;

	private TextField<Integer> hoursField;

	private Integer minutes;

	private TextField<Integer> minutesField;

	public BaseTimeField(String id)
	{
		this(id, null);
	}

	public BaseTimeField(String id, IModel<T> model)
	{
		super(id, model);

		hoursField = newIntegerField("hours", new PropertyModel<Integer>(this, "hours"), Application.get()
			.getConverterLocator().getConverter(Integer.class));
		hoursField.add(new RangeValidator<Integer>(0, 23));
		hoursField.setLabel(new Model<String>("hours"));
		add(hoursField);
		minutesField = newIntegerField("minutes", new PropertyModel<Integer>(this, "minutes"), MINUTES_CONVERTER);
		minutesField.add(new RangeValidator<Integer>(0, 59));
		minutesField.setLabel(new Model<String>("minutes"));
		add(minutesField);
	}

	protected TextField<Integer> newIntegerField(String wicketId, IModel<Integer> model,
		final IConverter<Integer> converter)
	{
		return new TextField<Integer>(wicketId, model, Integer.class)
		{

			private static final long serialVersionUID = 1L;

			@SuppressWarnings("unchecked")
			@Override
			public <C> IConverter<C> getConverter(Class<C> type)
			{
				if (type.equals(Integer.class))
				{
					return (IConverter<C>) converter;
				}
				return super.getConverter(type);
			}
		};
	}

	@Override
	public String getInput()
	{
		return hoursField.getInput() + ":" + minutesField.getInput();
	}

	@Override
	protected void onConfigure()
	{
		super.onConfigure();

		hoursField.setRequired(isRequired());
		minutesField.setRequired(isRequired());
	}

	protected TimeZone getClientTimeZone()
	{
		ClientInfo info = Session.get().getClientInfo();
		if (info instanceof WebClientInfo)
		{
			return ((WebClientInfo) info).getProperties().getTimeZone();
		}
		return null;
	}

	public Integer getHours()
	{
		return hours;
	}

	public void setHours(Integer hours)
	{
		this.hours = hours;
	}

	public Integer getMinutes()
	{
		return minutes;
	}

	public void setMinutes(Integer minutes)
	{
		this.minutes = minutes;
	}

	protected TextField<Integer> getHoursField()
	{
		return hoursField;
	}

	protected TextField<Integer> getMinutesField()
	{
		return minutesField;
	}
}
