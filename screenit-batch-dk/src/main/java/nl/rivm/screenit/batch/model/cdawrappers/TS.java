package nl.rivm.screenit.batch.model.cdawrappers;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import nl.rivm.screenit.Constants;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TS extends nl.rivm.screenit.hl7v3.cda.TS
{

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(TS.class);

	public TS(Date date)
	{
		this(date, false);
	}

	public TS(Date date, boolean seconden)
	{
		if (seconden)
		{
			setValue(new SimpleDateFormat(Constants.DATE_FORMAT_YYYYMMDDHHMMSS).format(date));
		}
		else
		{
			setValue(new SimpleDateFormat(Constants.DATE_FORMAT_YYYYMMDDHHMM).format(date));
		}
	}

	public static Date value(nl.rivm.screenit.hl7v3.cda.TS ts)
	{
		Date date = null;
		if (ts != null && ts.getValue() != null)
		{
			try
			{
				date = new SimpleDateFormat(Constants.DATE_FORMAT_YYYYMMDDHHMMSS).parse(ts.getValue());
			}
			catch (ParseException e0)
			{
				try
				{
					date = new SimpleDateFormat(Constants.DATE_FORMAT_YYYYMMDDHHMM).parse(ts.getValue());
				}
				catch (ParseException e1)
				{
					try
					{
						date = new SimpleDateFormat(Constants.DATE_FORMAT_YYYYMMDD).parse(ts.getValue());
					}
					catch (ParseException e2)
					{
						LOG.error(e2.getMessage(), e2);
						throw new IllegalArgumentException("Fout bij parsen datum: " + ts.getValue());
					}
				}
			}
		}
		return date;
	}
}
