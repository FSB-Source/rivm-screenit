package nl.rivm.screenit.main.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.function.UnaryOperator;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.comparator.BriefCreatieDatumComparator;
import nl.rivm.screenit.main.model.TypeGebeurtenis;
import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.functionalinterfaces.TriFunction;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.model.IModel;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class BriefOmschrijvingUtil
{
	public static List<String> getBrievenOmschrijvingen(List<? extends ClientBrief> brieven)
	{
		List<String> brievenStrings = new ArrayList<>();
		brieven.sort(new BriefCreatieDatumComparator());
		SimpleDateFormat formatter = new SimpleDateFormat("dd-MM-yyyy");
		for (var brief : brieven)
		{
			StringBuilder builder = new StringBuilder();
			builder.append(brief.getBriefType().getWeergaveNaam());
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
		omschrijving.append(brief.getBriefType().getWeergaveNaam());
		if (StringUtils.isNotBlank(brief.getTemplateNaam()))
		{
			omschrijving.append(", ");
			omschrijving.append(brief.getTemplateNaam());
		}
		if (brief instanceof BezwaarBrief && ((BezwaarBrief) brief).isVragenOmHandtekening())
		{
			omschrijving.append(" - ");
			omschrijving.append(getString.apply("label.formulier.handtekeningvergeten"));
		}
		omschrijving.append(")");
	}

	public static TypeGebeurtenis bepaalTypeGebeurtenis(Brief brief)
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

	public static String verwerkExtraOmschrijvingen(String[] extraOmschrijvingen, TriFunction<String, IModel<?>, String, String> getString)
	{
		String extraOmschrijving = "";
		if (extraOmschrijvingen == null)
		{
			return extraOmschrijving;
		}

		extraOmschrijvingen = Arrays.stream(extraOmschrijvingen).filter(Objects::nonNull).toArray(String[]::new);
		var index = 0;
		var aantal = extraOmschrijvingen.length;
		for (String omschrijving : extraOmschrijvingen)
		{
			if (StringUtils.isNotBlank(extraOmschrijving))
			{
				if (extraOmschrijving.trim().endsWith(":"))
				{
					if (!extraOmschrijving.endsWith(":"))
					{
						extraOmschrijving += " ";
					}
					extraOmschrijving += " ";
				}
				else if (index < aantal)
				{
					extraOmschrijving += ", ";
				}
			}
			else
			{
				extraOmschrijving = "(";
			}
			extraOmschrijving += getString.apply(omschrijving, null, omschrijving).trim();
			index++;
		}
		if (StringUtils.isNotBlank(extraOmschrijving))
		{
			extraOmschrijving += ")";
		}
		return extraOmschrijving;
	}
}
