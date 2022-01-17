
package nl.rivm.screenit.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DecimalFormatSymbols;
import java.util.Locale;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.IGeografischeCoordinaten;

public final class BigDecimalUtil
{

	private static final BigDecimal HALF = new BigDecimal(.5);

	private BigDecimalUtil()
	{

	}

	public static double berekenDistance(IGeografischeCoordinaten from, IGeografischeCoordinaten to)
	{
		return berekenDistance(from.getLatitude(), from.getLongitude(), to.getLatitude(), to.getLongitude());
	}

	public static double berekenDistance(BigDecimal latitudeFrom, BigDecimal longitudeFrom, BigDecimal latitudeTo, BigDecimal longitudeTo)
	{
		double latTo = latitudeTo.doubleValue() * Constants.RADIANS;
		double lngTo = longitudeTo.doubleValue() * Constants.RADIANS;

		double latFrom = latitudeFrom.doubleValue() * Constants.RADIANS;
		double lngFrom = longitudeFrom.doubleValue() * Constants.RADIANS;

		double dLng = lngTo - lngFrom;
		double dLat = latTo - latFrom;
		double a = Math.pow(Math.sin(dLat / 2), 2) + Math.cos(latFrom) * Math.cos(latTo) * Math.pow(Math.sin(dLng / 2), 2);
		double intermediateResult = 2 * Math.asin(Math.min(1, Math.sqrt(a)));
		return Constants.EARTH_RADIUS * intermediateResult / 1000.0;
	}

	public static String decimalToString(BigDecimal decimal)
	{
		return decimalToString(decimal, null);
	}

	public static String decimalToString(BigDecimal decimal, Integer scale)
	{
		if (decimal == null)
		{
			return "<geen waarde>";
		}
		else if (decimal.compareTo(BigDecimal.ZERO) == 0)
		{
			return decimal.scale() <= 0 ? "0" : "0.0";
		}
		else
		{
			if (scale != null)
			{
				decimal = decimal.setScale(scale, RoundingMode.HALF_UP);
			}
			return decimal.stripTrailingZeros().toPlainString();
		}
	}

	public static Integer roundCapaciteit(BigDecimal capaciteit)
	{
		if (capaciteit == null)
		{
			return null;
		}
		BigDecimal roundedCapaciteit = capaciteit.setScale(0, RoundingMode.HALF_UP);
		if (capaciteit.compareTo(BigDecimal.ZERO) > 0 && capaciteit.compareTo(new BigDecimal("0.5")) < 0)
		{
			roundedCapaciteit = BigDecimal.ONE;
		}
		return roundedCapaciteit.intValue();
	}

	public static BigDecimal roundToNearestHalf(BigDecimal bigDecimal)
	{
		if (bigDecimal == null)
		{
			return null;
		}
		return bigDecimal.divide(HALF, 0, BigDecimal.ROUND_HALF_UP).multiply(HALF).setScale(1);
	}

	public static BigDecimal stringToBigDecimal(final String formattedString, final Locale locale)
	{
		DecimalFormatSymbols symbols = new DecimalFormatSymbols(locale);
		char groupSeparatorChar = symbols.getGroupingSeparator();
		char decimalSeparatorChar = symbols.getDecimalSeparator();
		String groupSeparator;
		String decimalSeparator;

		if (groupSeparatorChar == '.')
		{
			groupSeparator = "\\" + groupSeparatorChar;
		}
		else
		{
			groupSeparator = Character.toString(groupSeparatorChar);
		}

		if (decimalSeparatorChar == '.')
		{
			decimalSeparator = "\\" + decimalSeparatorChar;
		}
		else
		{
			decimalSeparator = Character.toString(decimalSeparatorChar);
		}

		String fixedString = formattedString.replaceAll(groupSeparator, "");
		fixedString = fixedString.replaceAll(decimalSeparator, ".");

		return new BigDecimal(fixedString);
	}

}
