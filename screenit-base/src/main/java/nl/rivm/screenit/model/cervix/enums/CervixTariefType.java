package nl.rivm.screenit.model.cervix.enums;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import lombok.AllArgsConstructor;
import lombok.Getter;

import nl.rivm.screenit.model.INaam;
import nl.rivm.screenit.model.cervix.facturatie.CervixHuisartsTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixLabTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixTarief;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;

@AllArgsConstructor
@Getter
public enum CervixTariefType implements INaam
{

	LAB_HPV_ANALYSE_UITSTRIJKJE("hpvAnalyseUitstrijkjeTarief", "HPV analyse uitstrijkje", false),

	LAB_HPV_ANALYSE_ZAS("hpvAnalyseZasTarief", "HPV analyse ZAS", false),

	LAB_CYTOLOGIE_NA_HPV_UITSTRIJKJE("cytologieNaHpvUitstrijkjeTarief", "Cytologie uitstrijkje HPV(+)", false),

	LAB_CYTOLOGIE_NA_HPV_ZAS("cytologieNaHpvZasTarief", "Cytologie ZAS HPV(+)", false),

	LAB_CYTOLOGIE_VERVOLGUITSTRIJKJE("cytologieVervolguitstrijkjeTarief", "Cytologie vervolguitstrijkje", false),

	HUISARTS_UITSTRIJKJE("tarief", "Uitstrijkje", false),

	LAB_LOGISTIEK("logistiekTarief", "P1 Logistiek", true),

	LAB_MONSTERONTVANGST_EN_MONSTEROPWERKING_ZAS("monsterontvangstEnMonsterverwerkingZasTarief", "P2 Monsterontvangst & Monsteropwerking ZAS", true),

	LAB_HPV_ANALYSE_KLINISCH_EN_ZELF_AFGENOMEN("hpvAnalyseKlinischEnZelfAfgenomenTarief", "P3 HPV-analyse Klinisch & zelf afgenomen", true),

	LAB_CERVIXCYTOLOGIE_MANUEEL_SCREENEN("cervixcytologieManueelScreenenTarief", "P4a Cervixcytologie manueel screenen", true),

	LAB_CERVIXCYTOLOGIE_MET_COS("cervixcytologieMetCosTarief", "P4b Cervixcytologie met COS", true);

	private final String bedragProperty;

	private final String naam;

	private final boolean bmhk2023Lab;

	public static List<CervixTariefType> getAlleLabTariefTypes()
	{
		return Arrays.stream(values())
			.filter(tarief -> tarief != HUISARTS_UITSTRIJKJE)
			.collect(Collectors.toList());
	}

	public static List<CervixTariefType> getAlleLabTariefTypes(boolean bmhk2023Lab)
	{
		return getAlleLabTariefTypes().stream()
			.filter(tarief -> tarief.isBmhk2023Lab() == bmhk2023Lab)
			.collect(Collectors.toList());
	}

	public static List<CervixTariefType> getAlleHuisartsTariefTypes()
	{
		return List.of(HUISARTS_UITSTRIJKJE);
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
			case LAB_LOGISTIEK:
				return getLabTarief(tarief).getLogistiekTarief();
			case LAB_MONSTERONTVANGST_EN_MONSTEROPWERKING_ZAS:
				return getLabTarief(tarief).getMonsterontvangstEnMonsterverwerkingZasTarief();
			case LAB_HPV_ANALYSE_KLINISCH_EN_ZELF_AFGENOMEN:
				return getLabTarief(tarief).getHpvAnalyseKlinischEnZelfAfgenomenTarief();
			case LAB_CERVIXCYTOLOGIE_MANUEEL_SCREENEN:
				return getLabTarief(tarief).getCervixcytologieManueelScreenenTarief();
			case LAB_CERVIXCYTOLOGIE_MET_COS:
				return getLabTarief(tarief).getCervixcytologieMetCosTarief();
			case HUISARTS_UITSTRIJKJE:
				return getHuisartsTarief(tarief).getTarief();
			default:
				throw new IllegalStateException(String.format("Geen bedrag gevonden voor tarief %s", name()));
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

	@Override
	public String getNaam()
	{
		return naam;
	}

	public static List<CervixTariefType> CYTOLOGIE_TARIEF_TYPES = List.of(LAB_CYTOLOGIE_NA_HPV_ZAS, LAB_CYTOLOGIE_NA_HPV_UITSTRIJKJE, LAB_CYTOLOGIE_VERVOLGUITSTRIJKJE,
		LAB_CERVIXCYTOLOGIE_MET_COS,
		LAB_CERVIXCYTOLOGIE_MANUEEL_SCREENEN);
}
