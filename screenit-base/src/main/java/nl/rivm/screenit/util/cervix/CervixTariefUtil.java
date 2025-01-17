package nl.rivm.screenit.util.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.math.BigDecimal;

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

	public static void vulTarief(CervixLabTarief tarief, CervixLabTarief laatste)
	{
		if (laatste != null)
		{

			tarief.setHpvAnalyseZasTarief(laatste.getHpvAnalyseZasTarief());
			tarief.setHpvAnalyseUitstrijkjeTarief(laatste.getHpvAnalyseUitstrijkjeTarief());
			tarief.setCytologieNaHpvUitstrijkjeTarief(laatste.getCytologieNaHpvUitstrijkjeTarief());
			tarief.setCytologieNaHpvZasTarief(laatste.getCytologieNaHpvZasTarief());
			tarief.setCytologieVervolguitstrijkjeTarief(laatste.getCytologieVervolguitstrijkjeTarief());

			tarief.setLogistiekTarief(laatste.getLogistiekTarief());
			tarief.setMonsterontvangstEnMonsterverwerkingZasTarief(laatste.getMonsterontvangstEnMonsterverwerkingZasTarief());
			tarief.setHpvAnalyseKlinischEnZelfAfgenomenTarief(laatste.getHpvAnalyseKlinischEnZelfAfgenomenTarief());
			tarief.setCervixcytologieManueelScreenenTarief(laatste.getCervixcytologieManueelScreenenTarief());
			tarief.setCervixcytologieMetCosTarief(laatste.getCervixcytologieMetCosTarief());
		}
		else
		{

			tarief.setHpvAnalyseZasTarief(BigDecimal.ZERO);
			tarief.setHpvAnalyseUitstrijkjeTarief(BigDecimal.ZERO);
			tarief.setCytologieNaHpvUitstrijkjeTarief(BigDecimal.ZERO);
			tarief.setCytologieNaHpvZasTarief(BigDecimal.ZERO);
			tarief.setCytologieVervolguitstrijkjeTarief(BigDecimal.ZERO);

			tarief.setLogistiekTarief(BigDecimal.ZERO);
			tarief.setMonsterontvangstEnMonsterverwerkingZasTarief(BigDecimal.ZERO);
			tarief.setHpvAnalyseKlinischEnZelfAfgenomenTarief(BigDecimal.ZERO);
			tarief.setCervixcytologieManueelScreenenTarief(BigDecimal.ZERO);
			tarief.setCervixcytologieMetCosTarief(BigDecimal.ZERO);
		}
	}
}
