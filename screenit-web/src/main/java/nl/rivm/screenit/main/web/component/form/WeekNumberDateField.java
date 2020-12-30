package nl.rivm.screenit.main.web.component.form;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.temporal.WeekFields;
import java.util.Locale;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.util.DateUtil;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;
import org.apache.wicket.model.IModel;
import org.apache.wicket.util.convert.ConversionException;
import org.apache.wicket.util.convert.IConverter;
import org.wicketstuff.wiquery.core.javascript.JsStatement;
import org.wicketstuff.wiquery.ui.datepicker.DateOption;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;
import org.wicketstuff.wiquery.ui.datepicker.scope.JsScopeUiDatePickerDateTextEvent;

public class WeekNumberDateField extends DatePicker<LocalDate>
{

	public WeekNumberDateField(String id, IModel<LocalDate> dateModel, LocalDate eersteDagNieuweWeek)
	{
		super(id, dateModel);
		setType(LocalDate.class);
		setShowOn(ShowOnEnum.FOCUS);
		setButtonImageOnly(false);
		setShowWeek(true);

		if (eersteDagNieuweWeek != null)
		{
			setMinDate(new DateOption(DateUtil.toUtilDate(eersteDagNieuweWeek)));
		}

		JsScopeUiDatePickerDateTextEvent onSelectEventJs = JsScopeUiDatePickerDateTextEvent
			.quickScope(new JsStatement()
				.append("var selectedDate = $(this).datepicker('getDate');")
				.append(" var year = selectedDate.getFullYear(); ")
				.append("var weekOfYear = $.datepicker.iso8601Week(selectedDate); ")
				.append("if (selectedDate.getMonth() === 0 && weekOfYear > 51) { ")
				.append("year = year - 1; } ")
				.append("inst.input.val('Week ' + weekOfYear + ', ' +  year); ")
				.append("inst.input.trigger(\'change\');"));

		setOnSelectEvent(onSelectEventJs);
	}

	@Override
	protected IConverter<?> createConverter(Class<?> type)
	{
		return new IConverter<LocalDate>()
		{
			@Override
			public LocalDate convertToObject(String value, Locale locale) throws ConversionException
			{
				if (StringUtils.isNotBlank(value))
				{
					String weeknummer = StringUtils.removeStart(value, "Week ");
					String[] splittedWeeknummer = weeknummer.split(",");
					if (splittedWeeknummer.length == 2 && NumberUtils.isDigits(StringUtils.trim(splittedWeeknummer[0]))
						&& NumberUtils.isDigits(StringUtils.trim(splittedWeeknummer[1])))
					{
						int weekNr = Integer.valueOf(StringUtils.trim(splittedWeeknummer[0]));
						int jaar = Integer.valueOf(StringUtils.trim(splittedWeeknummer[1]));
						return LocalDate.now().withYear(jaar).with(WeekFields.of(Constants.LOCALE_NL).weekOfYear(), weekNr).with(DayOfWeek.MONDAY);
					}
					throw new ConversionException("");
				}
				return null;
			}

			@Override
			public String convertToString(LocalDate value, Locale locale)
			{
				if (value != null)
				{
					return "Week " + DateUtil.getWeekNr(value) + ", " + value.getYear();
				}
				return null;
			}
		};
	}
}
