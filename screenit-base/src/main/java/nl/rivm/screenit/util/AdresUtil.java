package nl.rivm.screenit.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.TijdelijkAdres;
import nl.rivm.screenit.model.TijdelijkGbaAdres;
import nl.rivm.screenit.model.cervix.CervixHuisartsAdres;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.topicuszorg.organisatie.model.Adres;
import nl.topicuszorg.util.postcode.PostcodeFormatter;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.Hibernate;

import com.google.common.base.Strings;
import com.google.common.collect.Range;

public final class AdresUtil
{

	public static final Pattern POSTCODE_NL = Pattern.compile("([1-9][0-9]{3}?)[\\s]?([a-zA-Z]{2})");

	private AdresUtil()
	{

	}

	public static String getVolledigeGbaAdresString(GbaPersoon persoon)
	{
		Adres adres = persoon.getGbaAdres();

		if (adres == null)
		{
			return "";
		}

		return getVolledigeAdresString(adres);
	}

	public static String getVolledigeAdresString(Adres adres)
	{

		StringBuilder adresString = new StringBuilder();
		if (adres != null)
		{
			adresString.append(getAdres(adres));

			String postcode = adres.getPostcode();
			String plaats = getTeGebruikenWoonplaats(adres);
			if (adresString.length() > 0 && (StringUtils.isNotBlank(postcode) || StringUtils.isNotBlank(plaats)))
			{
				adresString.append(",");
			}

			if (StringUtils.isNotBlank(postcode))
			{
				if (adresString.length() > 0)
				{
					adresString.append(" ");
				}
				adresString.append(PostcodeFormatter.formatPostcode(postcode, true));
			}
			if (StringUtils.isNotBlank(plaats))
			{
				if (adresString.length() > 0)
				{
					adresString.append(" ");
				}
				adresString.append(plaats);
			}
		}
		return adresString.toString();
	}

	private static String getTeGebruikenWoonplaats(Adres adres)
	{
		if (adres instanceof CervixHuisartsAdres)
		{
			CervixHuisartsAdres cervixAdres = (CervixHuisartsAdres) adres;
			return cervixAdres.getWoonplaats() != null ? cervixAdres.getWoonplaats().getNaam() : "";
		}

		return adres.getPlaats();
	}

	public static String getHuisnummerVolledig(Adres adres)
	{
		StringBuilder adresString = new StringBuilder();
		if (adres.getHuisnummer() != null)
		{
			adresString.append(adres.getHuisnummer());
		}

		if (!Strings.isNullOrEmpty(adres.getHuisletter()))
		{
			if (adresString.length() > 0)
			{
				adresString.append(" ");
			}
			adresString.append(adres.getHuisletter());
		}

		if (!Strings.isNullOrEmpty(adres.getHuisnummerToevoeging()))
		{
			if (adresString.length() > 0)
			{
				adresString.append(" ");
			}
			adresString.append(adres.getHuisnummerToevoeging());
		}

		if (!Strings.isNullOrEmpty(adres.getHuisnummerAanduiding()))
		{
			if (adresString.length() > 0)
			{
				adresString.append(" ");
			}
			adresString.append(adres.getHuisnummerAanduiding());
		}
		return adresString.toString();
	}

	public static String getAdres(Adres adres)
	{

		StringBuilder adresString = new StringBuilder();
		if (adres != null)
		{
			if (!Strings.isNullOrEmpty(adres.getStraat()))
			{
				adresString.append(adres.getStraat());
				adresString.append(" ");
				adresString.append(getHuisnummerVolledig(adres));
			}
			else if (!Strings.isNullOrEmpty(adres.getLocatieBeschrijving()))
			{
				adresString.append(adres.getLocatieBeschrijving());
			}
		}
		return adresString.toString();
	}

	public static boolean isZelfdeStandplaatsLocatie(MammaStandplaatsLocatie adres1, MammaStandplaatsLocatie adres2)
	{
		return adres1.getId().equals(adres2.getId()) || getAdresVoorStandplaatsLocatie(adres1, true).equals(getAdresVoorStandplaatsLocatie(adres2, true));
	}

	public static String getAdresVoorStandplaatsLocatie(MammaStandplaatsLocatie adres)
	{
		return getAdresVoorStandplaatsLocatie(adres, false);
	}

	private static String getAdresVoorStandplaatsLocatie(MammaStandplaatsLocatie adres, boolean huisnummerTonenVerplicht)
	{
		StringBuilder adresString = new StringBuilder();
		if (adres != null)
		{
			adresString.append(getStraatMetHuisnummerVoorStandplaatsLocatie(adres, huisnummerTonenVerplicht));

			String postcode = adres.getPostcode();
			String plaats = getTeGebruikenWoonplaats(adres);
			if (adresString.length() > 0 && (StringUtils.isNotBlank(postcode) || StringUtils.isNotBlank(plaats)))
			{
				adresString.append(",");
			}

			if (StringUtils.isNotBlank(postcode))
			{
				if (adresString.length() > 0)
				{
					adresString.append(" ");
				}
				adresString.append(PostcodeFormatter.formatPostcode(postcode, true));
			}
			if (StringUtils.isNotBlank(plaats))
			{
				if (adresString.length() > 0)
				{
					adresString.append(" ");
				}
				adresString.append(plaats);
			}
		}
		return adresString.toString();
	}

	public static String getStraatMetHuisnummerVoorStandplaatsLocatie(MammaStandplaatsLocatie adres, boolean huisnummerTonenVerplicht)
	{
		StringBuilder straatString = new StringBuilder();
		if (adres != null)
		{
			if (StringUtils.isNotBlank(adres.getStraat()))
			{
				straatString.append(adres.getStraat());
				if (huisnummerTonenVerplicht || Boolean.TRUE.equals(adres.getToonHuisnummerInBrieven()))
				{
					straatString.append(" ");
					straatString.append(getHuisnummerVolledig(adres));
				}
			}
		}
		return straatString.toString();
	}

	public static boolean isTijdelijkAdres(GbaPersoon persoon, LocalDate date)
	{
		return getAdres(persoon, date) instanceof TijdelijkAdres;
	}

	public static Adres getAdres(GbaPersoon persoon, LocalDate date)
	{
		Adres adres;
		var tijdelijkAdres = persoon.getTijdelijkAdres();
		if (tijdelijkAdres == null)
		{
			adres = persoon.getGbaAdres();
		}
		else
		{
			var startDatum = DateUtil.toLocalDate(tijdelijkAdres.getStartDatum());
			var eindDatum = DateUtil.toLocalDate(tijdelijkAdres.getEindDatum());

			if (StringUtils.isBlank(tijdelijkAdres.getPostcode()) || tijdelijkAdres.getHuisnummer() == null)
			{
				adres = persoon.getGbaAdres();
			}
			else if (StringUtils.isBlank(tijdelijkAdres.getPlaats()) || StringUtils.isBlank(tijdelijkAdres.getStraat()))
			{
				adres = persoon.getGbaAdres();
			}
			else if (startDatum == null && eindDatum == null)
			{
				adres = persoon.getGbaAdres();
			}
			else if (startDatum != null && eindDatum == null && startDatum.isAfter(date))
			{
				adres = persoon.getGbaAdres();
			}
			else if (startDatum == null && eindDatum.isBefore(date))
			{
				adres = persoon.getGbaAdres();
			}
			else if (startDatum != null && eindDatum != null && !Range.closed(startDatum, eindDatum).contains(date))
			{
				adres = persoon.getGbaAdres();
			}
			else
			{
				adres = tijdelijkAdres;
			}
		}
		TijdelijkGbaAdres tijdelijkGbaAdres = persoon.getTijdelijkGbaAdres();
		if (tijdelijkGbaAdres != null && BagAdres.class.equals(Hibernate.getClass(adres)))
		{
			adres = tijdelijkGbaAdres;
		}
		return adres;
	}

	public static boolean isOnvolledigAdres(Adres adres)
	{
		return adres != null && (StringUtils.isBlank(adres.getPostcode()) || adres.getHuisnummer() == null || StringUtils.isNotBlank(adres.getLocatieBeschrijving())
			|| StringUtils.isBlank(adres.getPlaats()));
	}

	public static String bepaalMissendeAdresgegevensString(Adres adres)
	{
		List<String> missendeAdresgegevens = new ArrayList<>();
		if (!straatHuisnummerVolledig(adres))
		{
			if (StringUtils.isBlank(adres.getStraat()))
			{
				missendeAdresgegevens.add("straat");
			}
			if (adres.getHuisnummer() == null)
			{
				missendeAdresgegevens.add("huisnummer");
			}
		}
		else if (!locatiebeschrijvingGevuld(adres))
		{
			missendeAdresgegevens.add("locatie beschrijving");
		}

		if (StringUtils.isBlank(adres.getPlaats()))
		{
			missendeAdresgegevens.add("plaatsnaam");
		}
		if (StringUtils.isBlank(adres.getPostcode()))
		{
			missendeAdresgegevens.add("postcode");
		}
		return StringUtils.join(missendeAdresgegevens, ", ");
	}

	public static String createKixCode(Adres adres)
	{
		StringBuilder kixcode = new StringBuilder();
		if (adres != null && org.apache.commons.lang.StringUtils.isNotBlank(adres.getPostcode()))
		{
			kixcode.append(org.apache.commons.lang.StringUtils.deleteWhitespace(adres.getPostcode()).replaceAll("[^A-Za-z0-9]", "").toUpperCase());
			if (adres.getHuisnummer() != null)
			{
				kixcode.append(adres.getHuisnummer());
			}

			if (org.apache.commons.lang.StringUtils.isNotBlank(adres.getHuisnummerToevoeging()))
			{
				kixcode.append("X");
				kixcode.append(adres.getHuisnummerToevoeging().replaceAll("[^A-Za-z0-9]", "").toUpperCase());
			}

			if (org.apache.commons.lang.StringUtils.isNotBlank(adres.getHuisnummerAanduiding()))
			{
				kixcode.append("X");
				kixcode.append(adres.getHuisnummerAanduiding().replaceAll("[^A-Za-z0-9]", "").toUpperCase());
			}

			if (org.apache.commons.lang.StringUtils.isNotBlank(adres.getHuisletter()))
			{
				kixcode.append("X");
				kixcode.append(adres.getHuisletter().toUpperCase());
			}

		}
		return kixcode.toString();
	}

	public static boolean isVolledigAdresVoorInpakcentrum(Client client)
	{
		Adres adres = getAdres(client.getPersoon(), LocalDate.now());
		return adres != null && postcodeWoonplaatsVolledig(adres) && (straatHuisnummerVolledig(adres) || locatiebeschrijvingGevuld(adres));
	}

	private static boolean postcodeWoonplaatsVolledig(Adres adres)
	{
		return StringUtils.isNotBlank(adres.getPostcode()) && StringUtils.isNotBlank(adres.getPlaats());
	}

	private static boolean straatHuisnummerVolledig(Adres adres)
	{
		return StringUtils.isNotBlank(adres.getStraat()) && adres.getHuisnummer() != null;
	}

	private static boolean locatiebeschrijvingGevuld(Adres adres)
	{
		return StringUtils.isNotBlank(adres.getLocatieBeschrijving());
	}
}
