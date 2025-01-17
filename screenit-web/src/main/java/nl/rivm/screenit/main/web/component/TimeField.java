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

import java.util.Date;
import java.util.TimeZone;

import org.apache.wicket.model.IModel;
import org.joda.time.DateTimeFieldType;
import org.joda.time.DateTimeZone;
import org.joda.time.MutableDateTime;

public class TimeField extends BaseTimeField<Date>
{

	private static final long serialVersionUID = 1L;

	private MutableDateTime date;

	public TimeField(String id)
	{
		this(id, null);
	}

	public TimeField(String id, IModel<Date> model)
	{
		super(id, model);
	}

	@Override
	protected void onConfigure()
	{
		super.onConfigure();

		setType(Date.class);

		Date d = (Date) getDefaultModelObject();
		if (d != null)
		{
			date = new MutableDateTime(d);
		}
		else
		{
			date = null;
			setHours(null);
			setMinutes(null);
		}

		if (date != null)
		{
			TimeZone zone = getClientTimeZone();

			if (zone != null)
			{
				date.setMillis(getMillis(TimeZone.getDefault(), zone, date.getMillis()));
			}

			setHours(Integer.valueOf(date.get(DateTimeFieldType.hourOfDay())));
			setMinutes(Integer.valueOf(date.getMinuteOfHour()));
		}
	}

	@Override
	public void convertInput()
	{
		MutableDateTime d = new MutableDateTime(getDefaultModelObject());
		Integer h = getHoursField().getConvertedInput();
		Integer m = getMinutesField().getConvertedInput();

		try
		{
			if (h != null)
			{
				d.set(DateTimeFieldType.hourOfDay(), h.intValue() % 24);

			}
			if (m != null)
			{
				d.setMinuteOfHour(m.intValue());
			}
			else
			{
				d.setMinuteOfHour(0);
			}

			TimeZone zone = getClientTimeZone();
			if (zone != null)
			{
				d.setMillis(getMillis(zone, TimeZone.getDefault(), d.getMillis()));
			}

			d.setSecondOfMinute(0);

			setConvertedInput(d.toDate());
		}
		catch (RuntimeException e)
		{
			invalid();
		}
	}

	private long getMillis(TimeZone to, TimeZone from, long instant)
	{
		return DateTimeZone.forTimeZone(from).getMillisKeepLocal(DateTimeZone.forTimeZone(to), instant);
	}

	public Date getDate()
	{
		if (date != null)
		{
			return date.toDate();
		}
		else
		{
			return null;
		}
	}

	public void setDate(Date date)
	{
		if (date == null)
		{
			this.date = null;
			setDefaultModelObject(null);
			setHours(null);
			setMinutes(null);
			return;
		}

		this.date = new MutableDateTime(date);
		setDefaultModelObject(date);

		Integer h = getHours();
		Integer m = getMinutes();

		if (h != null)
		{
			this.date.set(DateTimeFieldType.hourOfDay(), h.intValue() % 24);
			if (m != null)
			{
				this.date.setMinuteOfHour(m.intValue());
			}
			else
			{
				this.date.setMinuteOfHour(0);
			}
		}
		setDefaultModelObject(this.date.toDate());
	}
}
