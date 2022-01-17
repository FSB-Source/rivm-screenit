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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.IFOBTType;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.enums.BriefType;

import org.springframework.beans.support.PropertyComparator;

public class ColonScreeningRondeUtil
{

	private ColonScreeningRondeUtil()
	{

	}

	public static IFOBTTest getEersteGunstigeTest(ColonScreeningRonde ronde)
	{
		IFOBTTest eersteTestgMetGunstigeUitslag = null;
		for (IFOBTTest test : ronde.getIfobtTesten())
		{
			if (IFOBTTestUtil.isGunstig(test)
				&& IFOBTTestStatus.UITGEVOERD.equals(test.getStatus()) && IFOBTType.GOLD.equals(test.getType())
				&& (eersteTestgMetGunstigeUitslag == null || DateUtil.compareAfter(eersteTestgMetGunstigeUitslag.getStatusDatum(), test.getStatusDatum())))
			{
				eersteTestgMetGunstigeUitslag = test;
			}
		}
		return eersteTestgMetGunstigeUitslag;
	}

	public static IFOBTTest getEersteOngunstigeTest(ColonScreeningRonde ronde)
	{
		IFOBTTest eersteOngunstigeTest = null;
		for (IFOBTTest test : ronde.getIfobtTesten())
		{
			if (IFOBTTestUtil.isOngunstig(test)
				&& (eersteOngunstigeTest == null || DateUtil.toLocalDateTime(eersteOngunstigeTest.getStatusDatum()).isAfter(DateUtil.toLocalDateTime(test.getStatusDatum()))))
			{
				eersteOngunstigeTest = test;
			}
		}
		return eersteOngunstigeTest;
	}

	public static boolean zijnErActieveIfobts(ColonScreeningRonde ronde)
	{
		boolean activeIfobts = false;
		for (IFOBTTest test : ronde.getIfobtTesten())
		{
			if (IFOBTTestStatus.ACTIEF.equals(test.getStatus()) && IFOBTType.GOLD.equals(test.getType()))
			{
				activeIfobts = true;
				break;
			}
		}
		return activeIfobts;
	}

	public static boolean zijnErOngunstigeIfobts(ColonScreeningRonde ronde)
	{
		return getEersteOngunstigeTest(ronde) != null;
	}

	public static boolean heeftUitslagBrief(ColonScreeningRonde ronde)
	{
		return ronde.getBrieven().stream().anyMatch(brief -> BriefType.COLON_UITSLAG_BRIEVEN.contains(brief.getBriefType()));
	}

	public static boolean heeftBuitenDoelgroepBrief(ColonScreeningRonde ronde)
	{
		return ronde.getBrieven().stream().anyMatch(brief -> BriefType.COLON_UITSLAGBRIEF_ONBEOORDEELBAAR_BUITEN_DOELGROEP.equals(brief.getBriefType())
			|| BriefType.COLON_UITSLAGBRIEF_ONGUNSTIGE_BUITEN_DOELGROEP.equals(brief.getBriefType()));
	}

	public static boolean heeftAfgerondeVerslag(ColonScreeningRonde screeningRonde, VerslagType... types)
	{
		return screeningRonde.getVerslagen().stream()
			.anyMatch(v -> (types == null || types.length == 0 || Arrays.asList(types).contains(v.getType()))
				&& v.getStatus() == VerslagStatus.AFGEROND);
	}

	public static boolean magUitnodigingMetFitMaken(ColonDossier dossier, int aantalRondesUitnodigingsbriefZonderFit)
	{
		List<ColonScreeningRonde> rondes = new ArrayList<>(dossier.getScreeningRondes());
		Collections.sort(rondes, new PropertyComparator<>("creatieDatum", false, false));
		if (rondes.size() < aantalRondesUitnodigingsbriefZonderFit)
		{
			return true;
		}
		for (ColonScreeningRonde ronde : rondes)
		{
			for (IFOBTTest test : ronde.getIfobtTesten())
			{
				if (test.getAnalyseDatum() != null || IFOBTTestStatus.VERWIJDERD.equals(test.getStatus()))
				{
					return true;
				}
			}
			if (ronde.getLaatsteAfmelding() != null)
			{
				return true;
			}
			aantalRondesUitnodigingsbriefZonderFit--;
			if (aantalRondesUitnodigingsbriefZonderFit == 0)
			{
				return false;
			}
		}
		return true;
	}

	public static boolean isLaatsteScreeningRondeNietVerlopen(ColonScreeningRonde laatsteScreeningRonde)
	{
		return laatsteScreeningRonde != null && !(ScreeningRondeStatus.AFGEROND.equals(laatsteScreeningRonde.getStatus())
			&& Constants.RONDE_AFROND_REDEN_BUITEN_DOELGROEP.equals(laatsteScreeningRonde.getAfgerondReden()));
	}

	public static boolean isLaatsteScreeningRondGeldigEnAangemeld(ColonScreeningRonde laatsteScreeningRonde)
	{
		return isLaatsteScreeningRondeNietVerlopen(laatsteScreeningRonde) && Boolean.TRUE.equals(laatsteScreeningRonde.getAangemeld());
	}
}
