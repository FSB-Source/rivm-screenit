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

import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.Random;
import java.util.stream.Collectors;

import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonGeinterpreteerdeUitslag;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.IFOBTType;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class IFOBTTestUtil
{
	private static final Logger LOG = LoggerFactory.getLogger(IFOBTTestUtil.class);

	private IFOBTTestUtil()
	{

	}

	public static boolean isOngunstig(IFOBTTest test)
	{
		if (test != null && test.getGeinterpreteerdeUitslag() != null && test.getType().equals(IFOBTType.STUDIE))
		{
			return test.getGeinterpreteerdeUitslag().equals(ColonGeinterpreteerdeUitslag.ONGUNSTIG);
		}
		if (test == null || test.getUitslag() == null || test.getNormWaarde() == null)
		{
			LOG.trace("test, uitslag, normwaarde of geinterpreteerde uitslag (studietest) is null");
			return false;
		}
		return test.getUitslag().compareTo(test.getNormWaarde()) >= 0;
	}

	public static boolean isGunstig(IFOBTTest test)
	{
		if (test != null && test.getGeinterpreteerdeUitslag() != null && test.getType().equals(IFOBTType.STUDIE))
		{
			return test.getGeinterpreteerdeUitslag().equals(ColonGeinterpreteerdeUitslag.GUNSTIG);
		}
		if (test == null || test.getUitslag() == null || test.getNormWaarde() == null)
		{
			LOG.trace("test, uitslag of normwaarde is null");
			return false;
		}
		return test.getUitslag().compareTo(test.getNormWaarde()) < 0;
	}

	public static String getIfobtTestBarcode(String prefix, int length)
	{
		Random randomGenerator = new Random();

		return prefix + StringUtils.leftPad(Integer.toString(randomGenerator.nextInt(99999)), length, '0');
	}

	public static String getIfobtTestBarcode(Long value)
	{
		return Long.toHexString(value.longValue()).toUpperCase();
	}

	public static String getIfobtTestBarcode()
	{
		return getIfobtTestBarcode("TGD", 5);
	}

	public static IFOBTTest getIfobtTest(ColonUitnodiging uitnodiging)
	{
		IFOBTTest test = null;
		if (uitnodiging != null)
		{
			test = uitnodiging.getGekoppeldeTest();
		}
		return test;

	}

	public static ColonUitnodiging getUitnodiging(IFOBTTest buis)
	{
		ColonUitnodiging uitnodiging = buis.getColonUitnodiging();
		if (uitnodiging == null)
		{
			uitnodiging = buis.getColonUitnodigingExtra();
		}
		return uitnodiging;

	}

	public static IFOBTTestStatus getActieveIFOBTTestStatusNaHeraanmelding(ColonUitnodiging uitnodiging)
	{
		if (uitnodiging != null)
		{
			IFOBTTest test = uitnodiging.getGekoppeldeTest();

			if (test != null)
			{
				if (test.getUitslag() == null)
				{
					return IFOBTTestStatus.ACTIEF;
				}
				else
				{
					return IFOBTTestStatus.UITGEVOERD;
				}
			}
		}
		return null;
	}

	public static boolean isEnigeUitgevoerdeIfobtInZelfdeRonde(IFOBTTest buis)
	{
		return buis.getColonScreeningRonde().getIfobtTesten().stream().filter(t -> t.getStatus() == IFOBTTestStatus.UITGEVOERD).collect(Collectors.toList()).size() == 1;
	}

	public static boolean heeftMeerdereOngunstigeUitslagenInZelfdeRonde(IFOBTTest buis)
	{
		return buis.getColonScreeningRonde().getIfobtTesten().stream().filter(IFOBTTestUtil::isOngunstig).collect(Collectors.toList()).size() > 1;
	}

	public static boolean isLaatsteUitslagVanLaatsteRonde(ColonDossier dossier, Date statusDatumAangeklikteUitslag)
	{
		List<IFOBTTest> uitgevoerdeIfobtTesten = getAlleUitgevoerdeIfobtTestenInLaatsteRonde(dossier);
		Date laatsteStatusDatum = getLaatsteStatusDatumVanIfobtTesten(uitgevoerdeIfobtTesten);
		return DateUtil.compareEquals(statusDatumAangeklikteUitslag, laatsteStatusDatum);
	}

	private static List<IFOBTTest> getAlleUitgevoerdeIfobtTestenInLaatsteRonde(ColonDossier dossier)
	{
		return dossier.getLaatsteScreeningRonde().getIfobtTesten().stream().filter(ifobtTest -> ifobtTest != null && ifobtTest.getStatus().equals(IFOBTTestStatus.UITGEVOERD) && ifobtTest.getType().equals(IFOBTType.GOLD))
			.collect(Collectors.toList());
	}

	private static Date getLaatsteStatusDatumVanIfobtTesten(List<IFOBTTest> uitgevoerdeIfobtTesten)
	{
		return uitgevoerdeIfobtTesten.stream().map(IFOBTTest::getStatusDatum).filter(Objects::nonNull).max(Date::compareTo).orElse(null);
	}

	public static boolean heeftUitslag(IFOBTTest buis)
	{
		return buis.getUitslag() != null || buis.getGeinterpreteerdeUitslag() != null;
	}
}
