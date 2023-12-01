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

import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.Random;
import java.util.stream.Collectors;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonGeinterpreteerdeUitslag;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.IFOBTType;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class FITTestUtil
{
	private static final Logger LOG = LoggerFactory.getLogger(FITTestUtil.class);

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

	public static String getFITTestBarcode(String prefix, int length)
	{
		Random randomGenerator = new Random();

		return prefix + StringUtils.leftPad(Integer.toString(randomGenerator.nextInt(99999)), length, '0');
	}

	public static String getFITTestBarcode(Long value)
	{
		return Long.toHexString(value.longValue()).toUpperCase();
	}

	public static String getFITTestBarcode()
	{
		return getFITTestBarcode("TGD", 5);
	}

	public static IFOBTTest getFITTest(ColonUitnodiging uitnodiging)
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

	public static IFOBTTestStatus getActieveFITTestStatusNaHeraanmelding(ColonUitnodiging uitnodiging)
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

	public static boolean isEnigeUitgevoerdeFITInZelfdeRonde(IFOBTTest buis)
	{
		return buis.getColonScreeningRonde().getIfobtTesten().stream().filter(t -> t.getStatus() == IFOBTTestStatus.UITGEVOERD && t.getType() == IFOBTType.GOLD)
			.collect(Collectors.toList()).size() == 1;
	}

	public static boolean heeftMeerdereOngunstigeUitslagenInZelfdeRonde(IFOBTTest buis)
	{
		return buis.getColonScreeningRonde().getIfobtTesten().stream().filter(FITTestUtil::isOngunstig).collect(Collectors.toList()).size() > 1;
	}

	public static boolean isLaatsteUitslagVanLaatsteRonde(ColonDossier dossier, Date statusDatumAangeklikteUitslag)
	{
		List<IFOBTTest> uitgevoerdeIfobtTesten = getAlleUitgevoerdeFITTestenInLaatsteRonde(dossier);
		Date laatsteStatusDatum = getLaatsteStatusDatumVanFITTesten(uitgevoerdeIfobtTesten);
		return DateUtil.compareEquals(statusDatumAangeklikteUitslag, laatsteStatusDatum);
	}

	private static List<IFOBTTest> getAlleUitgevoerdeFITTestenInLaatsteRonde(ColonDossier dossier)
	{
		return dossier.getLaatsteScreeningRonde().getIfobtTesten().stream()
			.filter(ifobtTest -> ifobtTest != null && ifobtTest.getStatus().equals(IFOBTTestStatus.UITGEVOERD) && ifobtTest.getType().equals(IFOBTType.GOLD))
			.collect(Collectors.toList());
	}

	private static Date getLaatsteStatusDatumVanFITTesten(List<IFOBTTest> uitgevoerdeIfobtTesten)
	{
		return uitgevoerdeIfobtTesten.stream().map(IFOBTTest::getStatusDatum).filter(Objects::nonNull).max(Date::compareTo).orElse(null);
	}

	public static boolean heeftUitslag(IFOBTTest buis)
	{
		return buis.getUitslag() != null || buis.getGeinterpreteerdeUitslag() != null;
	}

	public static boolean heeftOngunstigeUitslagInLaatsteRonde(ColonDossier dossier)
	{
		return getAlleUitgevoerdeFITTestenInLaatsteRonde(dossier).stream().anyMatch(FITTestUtil::isOngunstig);
	}

	public static boolean heeftGunstigeUitslagInLaatsteRonde(ColonDossier dossier)
	{
		return getAlleUitgevoerdeFITTestenInLaatsteRonde(dossier).stream().anyMatch(FITTestUtil::isGunstig);
	}
}
