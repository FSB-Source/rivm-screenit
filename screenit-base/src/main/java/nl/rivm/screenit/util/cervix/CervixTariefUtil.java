package nl.rivm.screenit.util.cervix;

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

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.stream.Collectors;

import nl.rivm.screenit.model.cervix.enums.CervixTariefType;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.model.cervix.facturatie.CervixHuisartsTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixLabTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixTarief;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;

public class CervixTariefUtil
{
	private CervixTariefUtil()
	{
	}

	public static BigDecimal getTariefBedrag(CervixBoekRegel boekRegel)
	{
		if (hoortVerrichtingBijHuisarts(boekRegel))
		{
			return getHuisartsBedrag(boekRegel);
		}
		else
		{
			return getLabBedrag(boekRegel);
		}
	}

	public static BigDecimal getLabBedrag(CervixBoekRegel boekRegel)
	{
		CervixTariefType tariefType = boekRegel.getVerrichting().getType();
		CervixLabTarief tarief = (CervixLabTarief) HibernateHelper.deproxy(boekRegel.getTarief());
		return tariefType.getBedragVanTarief(tarief);
	}

	public static BigDecimal getHuisartsBedrag(CervixBoekRegel boekRegel)
	{
		CervixHuisartsTarief tarief = (CervixHuisartsTarief) HibernateHelper.deproxy(boekRegel.getTarief());
		return tarief.getTarief();
	}

	public static boolean hoortVerrichtingBijHuisarts(CervixBoekRegel boekregel)
	{
		return CervixTariefType.HUISARTS_UITSTRIJKJE.equals(boekregel.getVerrichting().getType());
	}

	public static String getTariefString(CervixTarief tarief)
	{
		String tariefTekst = "'";

		if (CervixTariefType.isHuisartsTarief(tarief))
		{
			tariefTekst += "huisartstarief " + CervixTariefType.HUISARTS_UITSTRIJKJE.getBedragStringVanTarief(tarief);
		}
		else
		{
			tariefTekst += Arrays.asList(CervixTariefType.getAlleLabTariefTypes()).stream()
				.map(t -> t.getNaam() + ": tarief " + t.getBedragStringVanTarief(tarief))
				.collect(Collectors.joining(", "));
		}
		if (tarief.getActief())
		{
			tariefTekst += " " + getGeldigheidMelding(tarief);
		}
		else
		{
			tariefTekst += " (verwijderd)";
		}
		tariefTekst += "'";
		return tariefTekst;
	}

	public static String getGeldigheidMelding(CervixTarief tarief)
	{
		String logMelding = " geldig vanaf " + DateUtil.formatShortDate(tarief.getGeldigVanafDatum());
		if (tarief.getGeldigTotenmetDatum() != null)
		{
			logMelding += " t/m "
				+ DateUtil.formatShortDate(tarief.getGeldigTotenmetDatum());
		}
		return logMelding;
	}

}
