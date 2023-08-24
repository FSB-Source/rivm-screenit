package nl.rivm.screenit.huisartsenportaal.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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

import java.util.Random;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class CodeGenerator
{

	private static final char[] characters = "abcdefghjkmnpqrstuvwxyzABCDEFGHJKMNPQRSTUVWKYZ23456789".toCharArray();

	private static final Random random = new Random();

	public static String genereerCode(int aantalCodeSegmenten, int segmentenLengte)
	{
		var code = new StringBuilder();
		for (int s = 0; s < aantalCodeSegmenten; s++)
		{
			if (s > 0)
			{
				code.append("-");
			}
			for (int l = 0; l < segmentenLengte; l++)
			{
				code.append(characters[random.nextInt(characters.length)]);
			}
		}
		return code.toString();
	}
}
