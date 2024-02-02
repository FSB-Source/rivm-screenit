package nl.rivm.screenit.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.math.BigDecimal;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import java.util.regex.Pattern;

import org.apache.commons.lang.StringUtils;

public class StringUtil
{
	private static final Pattern CONTROLS_PATTERN = Pattern.compile("[\\p{C}&&[^\r\n\t]]", Pattern.MULTILINE);

	private StringUtil()
	{
	}

	public static String literal2string(Object literal)
	{
		if (literal == null)
		{
			return "";
		}
		return StringUtils.capitalize(literal.toString().toLowerCase().replace('_', ' '));
	}

	public static String literals2string(List literals)
	{
		if (literals == null)
		{
			return "";
		}
		String result = "", separator = "";
		for (Object literal : literals)
		{
			result += separator + literal2string(literal);
			separator = ", ";
		}
		return result;
	}

	public static String toString(Collection<?> collection, int maxLen)
	{
		StringBuilder builder = new StringBuilder();
		builder.append("[");
		int i = 0;
		for (Iterator<?> iterator = collection.iterator(); iterator.hasNext() && i < maxLen; i++)
		{
			if (i > 0)
			{
				builder.append(", ");
			}
			Object next = iterator.next();
			if (next instanceof Entry)
			{
				Entry entry = (Entry) next;
				builder.append(entry.getKey());
				builder.append("=");
				Object value = entry.getValue();
				if (value instanceof BigDecimal)
				{
					BigDecimal decimal = (BigDecimal) value;
					builder.append(BigDecimalUtil.decimalToString(decimal));
				}
				else
				{
					builder.append(value);
				}
			}
			else
			{
				builder.append(next);
			}

		}
		builder.append("]");
		return builder.toString();
	}

	public static boolean isAlfabetKarakter(char karakter)
	{
		return karakter >= 'a' && karakter <= 'z' || karakter >= 'A' && karakter <= 'Z';
	}

	public static String kvp2String(String key, String value)
	{
		return "\"" + key + "\" : \"" + value + "\"";
	}

	public static String boolean2String(Boolean booleanVal)
	{
		return booleanVal == null ? "Null" : booleanVal ? "Ja" : "Nee";
	}

	public static String enumName2readableString(String name)
	{
		return name.substring(0, 1) + name.substring(1).replaceAll("_", " ").toLowerCase();
	}

	public static String utf8ToIso85591(String input)
	{
		ByteBuffer inputBuffer = ByteBuffer.wrap(input.getBytes());
		CharBuffer data = StandardCharsets.UTF_8.decode(inputBuffer);
		ByteBuffer outputBuffer = StandardCharsets.ISO_8859_1.encode(data);
		return new String(outputBuffer.array(), StandardCharsets.ISO_8859_1);
	}

	public static boolean containsControlCharacter(String str)
	{
		if (str == null)
		{
			return false;
		}
		return CONTROLS_PATTERN.matcher(str).find();
	}
}
