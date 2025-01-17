package nl.rivm.screenit.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Random;

public final class AntwoordFormulierUtil
{

	private AntwoordFormulierUtil()
	{

	}

	public static String getTestObjid()
	{
		StringBuilder barcode = new StringBuilder();

		barcode.append("TST-OBJID");

		Random randomGenerator = new Random();
		String barcodeCijfers = Integer.toString(randomGenerator.nextInt(99999));

		for (int i = 8; i > barcodeCijfers.length(); i--)
		{
			barcode.append("0");
		}

		barcode.append(barcodeCijfers);

		return barcode.toString();
	}
}
