package nl.rivm.screenit.main.util;

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

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.model.SKMLSchemaMapping;
import nl.rivm.screenit.model.colon.SKMLExternSchema;
import nl.topicuszorg.wicket.input.converters.MultiFormatDateConverter;

import org.apache.commons.lang.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.wicket.util.convert.ConversionException;

public class SKMLXlsUtil
{

	private SKMLXlsUtil()
	{
	}

	public static boolean isMappingCompleet(SKMLSchemaMapping mapping)
	{
		boolean compleet = false;
		if (mapping != null && mapping.getSkmljaar() > -1 && mapping.getSkmlronde() > -1 && mapping.getMonsterletter() > -1 && mapping.getDeadline() > -1)
		{
			compleet = true;
		}
		return compleet;
	}

	public static boolean isSKMLExternSchemaValide(SKMLExternSchema schema)
	{
		boolean correct = false;
		if (schema != null && schema.getJaar() != null && schema.getRonde() != null && schema.getLetter() != null && schema.getDeadline() != null)
		{
			correct = true;
		}
		return correct;
	}

	public static Integer getIntFromCell(Cell cell)
	{
		Integer waarde = null;
		try
		{
			try
			{
				Double doublevalue = cell.getNumericCellValue();
				if (doublevalue != null)
				{
					waarde = Integer.valueOf(doublevalue.intValue());
				}
			}
			catch (IllegalStateException ise)
			{
				String stringvalue = cell.getStringCellValue();
				if (StringUtils.isNotBlank(stringvalue))
				{
					waarde = Integer.valueOf(stringvalue);
				}
			}
		}
		catch (NumberFormatException nfe)
		{

		}

		return waarde;
	}

	public static String getStringFromCell(Cell cell)
	{
		String waarde = null;
		try
		{
			waarde = cell.getStringCellValue();
		}
		catch (IllegalStateException ise)
		{
			Double doublevalue = cell.getNumericCellValue();
			if (doublevalue != null)
			{
				waarde = String.valueOf(doublevalue);
			}
		}
		return waarde;
	}

	public static Date getDateFromCell(Cell cell)
	{
		Date dateValue = null;
		try
		{
			dateValue = cell.getDateCellValue();
		}
		catch (IllegalStateException ise)
		{
			String stringvalue = cell.getStringCellValue();
			if (StringUtils.isNotBlank(stringvalue))
			{
				try
				{
					MultiFormatDateConverter datumConverter = new MultiFormatDateConverter("dd-MM-yyyy",
						new String[] { "dd-MM-yyyy", Constants.DEFAULT_DATE_TIME_FORMAT_SHORT_YEAR, "dd-M-yyyy", "dd-M-yy", "d-MM-yyyy", "d-MM-yy", "d-M-yyyy", "d-M-yy", "ddMMyy",
							"ddMMyyyy", "dd/MM/yyyy" });
					dateValue = datumConverter.convertToObject(stringvalue, Constants.LOCALE_NL);
				}
				catch (ConversionException ce)
				{

				}
			}
		}
		return dateValue;
	}

	public static String formatEnValideerMonsterLetter(String string)
	{
		string = StringUtils.trim(string);
		string = StringUtils.upperCase(string);
		if (string != null && !string.matches("[A-P]"))
		{
			string = null;
		}
		return string;
	}
}
