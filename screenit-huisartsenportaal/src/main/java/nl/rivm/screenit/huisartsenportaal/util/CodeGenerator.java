package nl.rivm.screenit.huisartsenportaal.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2016 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

public final class CodeGenerator
{

	private static char[] characters = "abcdefghjkmnpqrstuvwxyzABCDEFGHJKMNPQRSTUVWKYZ23456789".toCharArray();

	private CodeGenerator()
	{

	}

	public static String genereerCode(int aantalCodeSegmenten, int segmentenLengte)
	{
		Random random = new Random();
		String code = "";
		for (int s = 0; s < aantalCodeSegmenten; s++)
		{
			if (s > 0)
			{
				code += "-";
			}
			for (int l = 0; l < segmentenLengte; l++)
			{
				code += characters[random.nextInt(characters.length)];
			}
		}
		return code;
	}
}
