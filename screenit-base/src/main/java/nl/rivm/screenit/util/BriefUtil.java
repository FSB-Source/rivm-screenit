package nl.rivm.screenit.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.lang.reflect.ParameterizedType;

import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.colon.ColonBrief;
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
		return brief.getBriefType().toString();
	}

	public static <B extends Brief, MB extends MergedBrieven<B>> Class<B> getBriefClass(MB mergedBrieven)
	{
		return (Class<B>) ((ParameterizedType) mergedBrieven.getClass().getGenericSuperclass()).getActualTypeArguments()[0];
	}

	public static boolean isOngunstigeUitslagBrief(ColonBrief bestaandeBrief)
	{
		BriefType briefType = bestaandeBrief.getBriefType();
		return briefType.equals(BriefType.COLON_UITNODIGING_INTAKE) || briefType.equals(BriefType.COLON_INTAKE_AFMELDING)
			|| briefType.equals(BriefType.COLON_INTAKE_GEWIJZIGD);
	}

	public static boolean isUitslagBrief(ColonBrief bestaandeBrief)
	{
		return isOngunstigeUitslagBrief(bestaandeBrief) || bestaandeBrief.getBriefType().equals(BriefType.COLON_GUNSTIGE_UITSLAG);
	}

	public static Brief getOrigineleBrief(Brief brief)
	{
		if (brief != null)
		{
			brief = (Brief) HibernateHelper.deproxy(brief);
		}
		if (brief instanceof ProjectBrief)
		{
			brief = ((ProjectBrief) brief).getBrief();
			if (brief != null)
			{
				brief = (ClientBrief<?, ?, ?>) HibernateHelper.deproxy(brief);
			}
		}
		return brief;
	}

}
