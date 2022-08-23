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

import java.util.Date;

import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;

public class BriefUtil
{
	private BriefUtil()
	{

	}

	public static String getBriefTypeNaam(Brief brief)
	{
		return brief.getBriefType() != null ? brief.getBriefType().name() : brief.getClass().getSimpleName();
	}

	public static Bevolkingsonderzoek[] getOnderzoekenUitBriefType(Brief brief)
	{
		return brief.getBriefType() != null ? brief.getBriefType().getOnderzoeken() : new Bevolkingsonderzoek[0];
	}

	public static boolean isOngunstigeUitslagBrief(ColonBrief bestaandeBrief)
	{
		BriefType briefType = bestaandeBrief.getBriefType();
		return briefType.equals(BriefType.COLON_UITNODIGING_INTAKE) || briefType.equals(BriefType.COLON_INTAKE_AFMELDING)
			|| briefType.equals(BriefType.COLON_INTAKE_GEWIJZIGD);
	}

	public static boolean isUitslagBrief(ColonBrief bestaandeBrief)
	{
		BriefType briefType = bestaandeBrief.getBriefType();
		return isOngunstigeUitslagBrief(bestaandeBrief) || briefType.equals(BriefType.COLON_GUNSTIGE_UITSLAG) || briefType.equals(BriefType.COLON_UITSLAGBRIEF_EXTRA_MONSTER);
	}

	public static Brief getOrigineleBrief(Brief brief)
	{
		if (brief != null)
		{
			brief = (Brief) HibernateHelper.deproxy(brief);
		}
		if (brief instanceof ProjectBrief && ((ProjectBrief) brief).getBrief() != null)
		{
			brief = (ClientBrief<?, ?, ?>) HibernateHelper.deproxy(((ProjectBrief) brief).getBrief());
		}
		return brief;
	}

	public static MergedBrieven getMergedBrieven(Brief brief)
	{
		brief = getBriefVoorPrintStatus(brief);
		if (brief != null)
		{
			return brief.getMergedBrieven();
		}
		return null;
	}

	public static boolean isMergedBrievenGeprint(Brief brief)
	{
		MergedBrieven<?> mergedBrieven = getMergedBrieven(brief);
		if (mergedBrieven != null)
		{
			return Boolean.TRUE.equals(mergedBrieven.getGeprint());
		}
		return false;
	}

	public static boolean isGegenereerd(Brief brief)
	{
		brief = getBriefVoorPrintStatus(brief);
		if (brief != null)
		{
			return brief.isGegenereerd();
		}
		return false;
	}

	public static boolean isNietGegenereerdEnNietVervangen(Brief brief)
	{
		return !isGegenereerd(brief) && !brief.isVervangen();
	}

	public static ClientBrief getHerdruk(ClientBrief brief)
	{
		brief = (ClientBrief) getOrigineleBrief(brief);
		if (brief != null)
		{
			return brief.getHerdruk();
		}
		return null;
	}

	public static boolean isHerdruk(ClientBrief brief)
	{
		brief = (ClientBrief) getOrigineleBrief(brief);
		if (brief != null)
		{
			return brief.getHerdruk() != null;
		}
		return getHerdruk(brief) != null;
	}

	public static Brief setTegenhouden(Brief brief, boolean tegenhouden)
	{
		brief = getBriefVoorPrintStatus(brief);
		if (brief != null)
		{
			brief.setTegenhouden(tegenhouden);
		}
		return brief;
	}

	public static boolean isTegengehouden(Brief brief)
	{
		brief = getBriefVoorPrintStatus(brief);
		if (brief != null)
		{
			return brief.isTegenhouden();
		}
		return false;
	}

	public static Brief getBriefVoorPrintStatus(Brief brief)
	{
		if (brief != null)
		{
			brief = (Brief) HibernateHelper.deproxy(brief);
			if (brief instanceof ClientBrief)
			{
				ProjectBrief projectBrief = ((ClientBrief<?, ?, ?>) brief).getProjectBrief();
				if (projectBrief != null)
				{
					brief = projectBrief;
				}
			}
			return brief;
		}
		return null;
	}

	public static Date geefDatumVoorGebeurtenisoverzicht(Brief brief)
	{
		MergedBrieven<?> mergedBrieven = getMergedBrieven(brief);
		if (BriefUtil.isGegenereerd(brief) && mergedBrieven != null)
		{
			if (mergedBrieven.getPrintDatum() != null)
			{
				return mergedBrieven.getPrintDatum();
			}
			else
			{
				return mergedBrieven.getCreatieDatum();
			}
		}
		else
		{
			return brief.getCreatieDatum();
		}
	}
}
