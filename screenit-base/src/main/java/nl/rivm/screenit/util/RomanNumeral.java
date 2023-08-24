
package nl.rivm.screenit.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

public class RomanNumeral
{

	private RomanNumeral()
	{
	}

	enum Numeral
	{
		I(1),
		IV(4),
		V(5),
		IX(9),
		X(10),
		XL(40),
		L(50),
		XC(90),
		C(100),
		CD(400),
		D(500),
		CM(900),
		M(1000);

		private final int weight;

		Numeral(int weight)
		{
			this.weight = weight;
		}
	};

	public static String toRoman(long n)
	{
		if (n <= 0)
		{
			throw new IllegalArgumentException();
		}

		StringBuilder buf = new StringBuilder();

		final Numeral[] values = Numeral.values();
		for (int i = values.length - 1; i >= 0; i--)
		{
			while (n >= values[i].weight)
			{
				buf.append(values[i]);
				n -= values[i].weight;
			}
		}
		return buf.toString();
	}

	public static Integer toInteger(String roman)
	{
		int result = 0;
		String uRoman = roman.toUpperCase(); 
		for (int i = 0; i < uRoman.length() - 1; i++)
		{
			if (decodeSingleRoman(uRoman.charAt(i)) < decodeSingleRoman(uRoman.charAt(i + 1)))
			{
				result -= decodeSingleRoman(uRoman.charAt(i));
			}
			else
			{
				result += decodeSingleRoman(uRoman.charAt(i));
			}
		}
		result += decodeSingleRoman(uRoman.charAt(uRoman.length() - 1));
		return result;
	}

	private static int decodeSingleRoman(char letter)
	{
		return Numeral.valueOf(letter + "").weight;
	}
}
