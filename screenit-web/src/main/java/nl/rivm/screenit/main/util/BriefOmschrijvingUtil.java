package nl.rivm.screenit.main.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import nl.rivm.screenit.comparator.BriefCreatieDatumComparator;
import nl.rivm.screenit.main.model.TypeGebeurtenis;
import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.MergedBrieven;

import nl.rivm.screenit.util.EnumStringUtil;
import org.apache.commons.lang.StringUtils;

public class BriefOmschrijvingUtil
{
	private BriefOmschrijvingUtil()
	{
	}

	public static List<String> getBrievenOmschrijvingen(List<? extends ClientBrief> brieven, Function<String, String> getString)
	{
		List<String> brievenStrings = new ArrayList<String>();
		Collections.sort(brieven, new BriefCreatieDatumComparator());
		SimpleDateFormat formatter = new SimpleDateFormat("dd-MM-yyyy");
		for (ClientBrief brief : brieven)
		{
			StringBuilder builder = new StringBuilder();
			builder.append(getString.apply(EnumStringUtil.getPropertyString(brief.getBriefType())));
			builder.append("(");
			builder.append(formatter.format(brief.getCreatieDatum()));
			if (brief.getHerdruk() != null)
			{
				builder.append(", herdruk van ").append(formatter.format(brief.getHerdruk().getCreatieDatum()));
			}
			if (brief.isTegenhouden())
			{
				builder.append(", tegengehouden");
			}
			if (brief.isVervangen())
			{
				builder.append(", vervangen");
			}
			builder.append("), ");
			brievenStrings.add(builder.toString());
		}
		return brievenStrings;
	}

	public static void addExtraOmschrijving(StringBuilder omschrijving, Brief brief, Function<String, String> getString)
	{
		TypeGebeurtenis gebeurtenis = bepaalTypeGebeurtenis(brief);

		omschrijving.append(" (");
		omschrijving.append(getString.apply("label.formulier." + gebeurtenis.name().toLowerCase()));
		omschrijving.append(": ");
		omschrijving.append(getString.apply(EnumStringUtil.getPropertyString(brief.getBriefType())));
		if (StringUtils.isNotBlank(brief.getTemplateNaam()))
		{
			omschrijving.append(", ");
			omschrijving.append(brief.getTemplateNaam());
		}
		omschrijving.append(")");
	}

	private static TypeGebeurtenis bepaalTypeGebeurtenis(Brief brief)
	{
		TypeGebeurtenis gebeurtenis;
		MergedBrieven mergedBrieven = brief.getMergedBrieven();
		if (isAfgedrukteMigratieBrief(brief, mergedBrieven))
		{
			gebeurtenis = TypeGebeurtenis.BRIEF_AFGEDRUKT;
		}
		else if (brief.isGegenereerd())
		{
			gebeurtenis = mergedBrieven.getPrintDatum() != null ? TypeGebeurtenis.BRIEF_AFGEDRUKT : TypeGebeurtenis.BRIEF_KLAARGEZET;
		}
		else if (brief.isTegenhouden())
		{
			gebeurtenis = TypeGebeurtenis.BRIEF_TEGENHOUDEN;
		}
		else
		{
			gebeurtenis = TypeGebeurtenis.BRIEF_AANGEMAAKT;
		}
		return gebeurtenis;
	}

	private static boolean isAfgedrukteMigratieBrief(Brief brief, MergedBrieven mergedBrieven)
	{
		return brief.isGegenereerd() && mergedBrieven == null;
	}
}
