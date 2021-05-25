package nl.rivm.screenit.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Random;

import nl.topicuszorg.util.bsn.BsnUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.math.DoubleMath;

public class TestBsnGenerator
{

	private static final Logger LOG = LoggerFactory.getLogger(TestBsnGenerator.class);

	private static Random randomNum = new Random();

	private TestBsnGenerator()
	{
	}

	public static String getValideBsn()
	{
		String bsn = "";

		do
		{
			bsn = "";
			int totaal = 0;
			for (int i = 9; i > 1; i--)
			{
				int random = randomNum.nextInt(9);
				bsn = bsn + random;
				totaal = totaal + random * i;
			}

			int lastNumber = getLastNumber(totaal);
			bsn = bsn + lastNumber;
		}
		while (!BsnUtils.isValidBSN(bsn));
		return bsn;
	}

	private static int getLastNumber(Integer totaal)
	{
		int lastNumber = 0;
		for (int i = 0; i < 10; i++)
		{
			if (calculateWithLastNumber(totaal, i))
			{
				lastNumber = i;
				break;
			}
		}
		return lastNumber;
	}

	private static boolean calculateWithLastNumber(Integer totaal, Integer lastNumber)
	{
		int value = (-1) * lastNumber;
		int waarde = totaal + value;

		double uitkomst = (double) waarde / (double) 11;

		LOG.debug("totaal: " + totaal + " Uitkomst: " + uitkomst + " lastNumber: " + lastNumber + " value-1:  " + value);

		return DoubleMath.isMathematicalInteger(uitkomst);
	}

}
