package nl.rivm.screenit.main.web.component;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.util.ThreadLocalDateFormat;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.util.convert.ConversionException;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wicketstuff.datetime.DateConverter;

public class MultiDateConverter extends DateConverter
{

	private static final long serialVersionUID = 1L;

	private static final String[] FORMATS = new String[] { "dd-MM-yy", "ddMMyy", "dMyy" };

	private static final Logger LOG = LoggerFactory.getLogger(MultiDateConverter.class);

	private final List<ThreadLocalDateFormat> patterns = new ArrayList<ThreadLocalDateFormat>();

	public MultiDateConverter()
	{
		super(false);

		for (String s : FORMATS)
		{
			patterns.add(new ThreadLocalDateFormat(s));
		}
	}

	@Override
	public Date convertToObject(String value, Locale locale)
	{
		Date parsed = null;

		if (StringUtils.isNotBlank(value))
		{
			Iterator<ThreadLocalDateFormat> iter = patterns.iterator();
			while (parsed == null && iter.hasNext())
			{
				try
				{
					SimpleDateFormat sdf = iter.next().get();
					sdf.setLenient(false);
					sdf.set2DigitYearStart(beginThisYear().minusYears(98).toDate());
					parsed = sdf.parse(value);
					LOG.debug("Gebruikte format: " + sdf.toPattern());
				}
				catch (ParseException e)
				{
					LOG.debug("Conversion niet gelukt, " + e.getMessage());
				}
			}

			if (parsed == null)
			{
				throw new ConversionException(String.format("Geen enkele date format kon invoer %s parsen", value));
			}
		}

		return parsed;
	}

	@Override
	public String convertToString(Date value, Locale locale)
	{
		if (value != null)
		{
			return Constants.getDateFormat().format(value);
		}

		return null;
	}

	@Override
	public String getDatePattern(Locale locale)
	{
		return Constants.DEFAULT_DATE_FORMAT;
	}

	@Override
	protected DateTimeFormatter getFormat(Locale locale)
	{
		return null;
	}

	private DateTime beginThisYear()
	{
		DateTime today = new DateTime();
		return new DateTime(today.getYear(), 01, 01, 0, 0);
	}
}
