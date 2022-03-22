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
import java.util.Date;
import java.util.List;
import java.util.function.UnaryOperator;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.comparator.BriefCreatieDatumComparator;
import nl.rivm.screenit.main.model.TypeGebeurtenis;
import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.EnumStringUtil;

import org.apache.commons.lang.StringUtils;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class BriefOmschrijvingUtil
{
	public static List<String> getBrievenOmschrijvingen(List<? extends ClientBrief> brieven, UnaryOperator<String> getString)
	{
		List<String> brievenStrings = new ArrayList<>();
		brieven.sort(new BriefCreatieDatumComparator());
		SimpleDateFormat formatter = new SimpleDateFormat("dd-MM-yyyy");
		for (var brief : brieven)
		{
			StringBuilder builder = new StringBuilder();
			builder.append(getString.apply(EnumStringUtil.getPropertyString(brief.getBriefType())));
			builder.append("(");
			builder.append(formatter.format(brief.getCreatieDatum()));
			var herdrukBrief = BriefUtil.getHerdruk(brief);
			if (herdrukBrief != null)
			{
				builder.append(", herdruk van ").append(formatter.format(herdrukBrief.getCreatieDatum()));
			}
			if (BriefUtil.isTegengehouden(brief))
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

	public static void addExtraOmschrijving(StringBuilder omschrijving, Brief brief, UnaryOperator<String> getString)
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

	public static String algemeneBriefOmschrijving(Brief brief, UnaryOperator<String> getString)
	{
		StringBuilder omschrijving = new StringBuilder();
		TypeGebeurtenis gebeurtenis = bepaalTypeGebeurtenis(brief);
		omschrijving.append(getString.apply("TypeGebeurtenis." + gebeurtenis.name()));
		omschrijving.append(" (");
		omschrijving.append(getString.apply(EnumStringUtil.getPropertyString(brief.getBriefType())));
		omschrijving.append(")");
		return omschrijving.toString();
	}

	public static Date dossierGebeurtenisDatum(Brief brief)
	{
		var mergedBrieven = BriefUtil.getMergedBrieven(brief);
		if (mergedBrieven != null)
		{
			return mergedBrieven.getPrintDatum() != null ? mergedBrieven.getPrintDatum() : mergedBrieven.getCreatieDatum();
		}
		return brief.getCreatieDatum();
	}

	private static TypeGebeurtenis bepaalTypeGebeurtenis(Brief brief)
	{
		MergedBrieven<?> mergedBrieven = BriefUtil.getMergedBrieven(brief);
		if (isAfgedrukteMigratieBrief(brief, mergedBrieven))
		{
			return TypeGebeurtenis.BRIEF_AFGEDRUKT;
		}
		else if (BriefUtil.isGegenereerd(brief))
		{
			return mergedBrieven.getPrintDatum() != null ? TypeGebeurtenis.BRIEF_AFGEDRUKT : TypeGebeurtenis.BRIEF_KLAARGEZET;
		}
		else if (BriefUtil.isTegengehouden(brief))
		{
			return TypeGebeurtenis.BRIEF_TEGENHOUDEN;
		}
		else
		{
			return TypeGebeurtenis.BRIEF_AANGEMAAKT;
		}
	}

	private static boolean isAfgedrukteMigratieBrief(Brief brief, MergedBrieven<?> mergedBrieven)
	{
		return BriefUtil.isGegenereerd(brief) && mergedBrieven == null;
	}
}
