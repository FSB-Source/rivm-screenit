package nl.rivm.screenit.model.cervix.enums;

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

import java.math.BigDecimal;
import java.text.NumberFormat;

import nl.rivm.screenit.model.INaam;
import nl.rivm.screenit.model.cervix.facturatie.CervixHuisartsTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixLabTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixTarief;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;

public enum CervixTariefType implements INaam
{

	LAB_HPV_ANALYSE_UITSTRIJKJE("hpvAnalyseUitstrijkjeTarief", "HPV analyse uitstrijkje"),

	LAB_HPV_ANALYSE_ZAS("hpvAnalyseZasTarief", "HPV analyse ZAS"),

	LAB_CYTOLOGIE_NA_HPV_UITSTRIJKJE("cytologieNaHpvUitstrijkjeTarief", "Cytologie uitstrijkje HPV(+)"),

	LAB_CYTOLOGIE_NA_HPV_ZAS("cytologieNaHpvZasTarief", "Cytologie ZAS HPV(+)"),

	LAB_CYTOLOGIE_VERVOLGUITSTRIJKJE("cytologieVervolguitstrijkjeTarief", "Cytologie vervolguitstrijkje"),

	HUISARTS_UITSTRIJKJE("tarief", "Uitstrijkje");

	private String bedragProperty;

	private String naam;

	public String getBedragProperty()
	{
		return bedragProperty;
	}

	CervixTariefType(String bedragProperty, String naam)
	{
		this.bedragProperty = bedragProperty;
		this.naam = naam;
	}

	public static CervixTariefType[] getAlleLabTariefTypes()
	{
		return new CervixTariefType[] {
			LAB_HPV_ANALYSE_UITSTRIJKJE,
			LAB_HPV_ANALYSE_ZAS,
			LAB_CYTOLOGIE_NA_HPV_UITSTRIJKJE,
			LAB_CYTOLOGIE_NA_HPV_ZAS,
			LAB_CYTOLOGIE_VERVOLGUITSTRIJKJE,
		};
	}

	public static CervixTariefType[] getAlleHuisartsTariefTypes()
	{
		return new CervixTariefType[] {
			HUISARTS_UITSTRIJKJE
		};
	}

	public BigDecimal getBedragVanTarief(CervixTarief tarief)
	{
		if (tarief != null)
		{
			switch (this)
			{
			case LAB_HPV_ANALYSE_UITSTRIJKJE:
				return getLabTarief(tarief).getHpvAnalyseUitstrijkjeTarief();
			case LAB_HPV_ANALYSE_ZAS:
				return getLabTarief(tarief).getHpvAnalyseZasTarief();
			case LAB_CYTOLOGIE_NA_HPV_UITSTRIJKJE:
				return getLabTarief(tarief).getCytologieNaHpvUitstrijkjeTarief();
			case LAB_CYTOLOGIE_NA_HPV_ZAS:
				return getLabTarief(tarief).getCytologieNaHpvZasTarief();
			case LAB_CYTOLOGIE_VERVOLGUITSTRIJKJE:
				return getLabTarief(tarief).getCytologieVervolguitstrijkjeTarief();
			case HUISARTS_UITSTRIJKJE:
				return getHuisartsTarief(tarief).getTarief();
			default:
				throw new IllegalStateException();
			}
		}
		else
		{
			return BigDecimal.ZERO;
		}
	}

	public String getBedragStringVanTarief(CervixTarief tarief)
	{
		return NumberFormat.getCurrencyInstance().format(getBedragVanTarief(tarief));
	}

	public static boolean isHuisartsTarief(CervixTarief tarief)
	{
		tarief = (CervixTarief) HibernateHelper.deproxy(tarief);
		return tarief instanceof CervixHuisartsTarief;
	}

	public static CervixLabTarief getLabTarief(CervixTarief tarief)
	{
		tarief = (CervixTarief) HibernateHelper.deproxy(tarief);
		if (tarief instanceof CervixLabTarief)
		{
			return (CervixLabTarief) tarief;
		}
		throw new IllegalArgumentException("tarief is niet van het type CervixLabTarief: " + tarief.getClass());
	}

	public static CervixHuisartsTarief getHuisartsTarief(CervixTarief tarief)
	{
		tarief = (CervixTarief) HibernateHelper.deproxy(tarief);
		if (tarief instanceof CervixHuisartsTarief)
		{
			return (CervixHuisartsTarief) tarief;
		}
		throw new IllegalArgumentException("tarief is niet van het type CervixHuisartsTarief");
	}

	@Override
	public String getNaam()
	{
		return naam;
	}
}
