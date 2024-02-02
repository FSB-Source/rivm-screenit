package nl.rivm.screenit.clientportaal.validators;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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

import java.time.LocalDate;
import java.util.Date;

import nl.rivm.screenit.model.TijdelijkAdres;
import nl.rivm.screenit.util.DateUtil;

import ca.uhn.hl7v2.util.StringUtil;

public class TijdelijkAdresValidator
{

	private final static String POSTCODE_PATTERN = "^[1-9][0-9]{3}?[A-RT-Z][A-Z]|[sS][BCE-RT-Z]$";

	private final static String HUISLETTER_PATTERN = "^[a-zA-Z]+$";

	private final static String HUISNUMMER_PATTERN = "^[0-9]*$";

	public static void validateTijdelijkAdres(TijdelijkAdres nieuwAdres, TijdelijkAdres vorigAdres, LocalDate vandaag)
	{
		if (!verplichteVeldenZijnGevuld(nieuwAdres, vorigAdres))
		{
			throw new IllegalStateException("Niet alle verplichte velden zijn gevuld");
		}
		if ((vorigAdres != null && !nieuwAdres.getStartDatum().equals(vorigAdres.getStartDatum())) && !startdatumLigtInToekomst(nieuwAdres.getStartDatum(), vandaag))
		{
			throw new IllegalStateException("Startdatum ligt in het verleden");
		}
		if (nieuwAdres.getEindDatum() != null && !startdatumLigtVoorEinddatum(nieuwAdres.getStartDatum(), nieuwAdres.getEindDatum()))
		{
			throw new IllegalStateException("Einddatum ligt voor startdatum");
		}
		if (!postcodeIsCorrect(nieuwAdres.getPostcode()))
		{
			throw new IllegalStateException("Postcode heeft niet de juiste format");
		}
		if (nieuwAdres.getHuisletter() != null && !huisletterIsCorrect(nieuwAdres.getHuisletter()))
		{
			throw new IllegalStateException("Huisletter heeft niet de juiste format");
		}
		if (!huisnummerIsCorrect(nieuwAdres.getHuisnummer()))
		{
			throw new IllegalStateException("Huisnummer heeft niet de juiste format");
		}
	}

	private static boolean verplichteVeldenZijnGevuld(TijdelijkAdres nieuwAdres, TijdelijkAdres vorigAdres)
	{
		boolean eindDatumIsGevuldIndienVerplicht;
		if (vorigAdres != null && vorigAdres.getEindDatum() == null)
		{
			eindDatumIsGevuldIndienVerplicht = true;
		}
		else
		{
			eindDatumIsGevuldIndienVerplicht = nieuwAdres.getEindDatum() != null;
		}

		return !StringUtil.isBlank(nieuwAdres.getStraat()) && nieuwAdres.getHuisnummer() != null
			&& !StringUtil.isBlank(nieuwAdres.getPostcode())
			&& !StringUtil.isBlank(nieuwAdres.getPlaats()) && nieuwAdres.getStartDatum() != null
			&& eindDatumIsGevuldIndienVerplicht;
	}

	private static boolean startdatumLigtVoorEinddatum(Date startdatum, Date einddatum)
	{
		return DateUtil.compareAfter(einddatum, startdatum);
	}

	private static boolean startdatumLigtInToekomst(Date startdatum, LocalDate vandaag)
	{
		return DateUtil.compareAfter(startdatum, DateUtil.toUtilDate(vandaag));
	}

	private static boolean postcodeIsCorrect(String postcode)
	{
		return postcode.matches(POSTCODE_PATTERN);
	}

	private static boolean huisletterIsCorrect(String huisletter)
	{
		return huisletter.matches(HUISLETTER_PATTERN);
	}

	private static boolean huisnummerIsCorrect(int huisnummer)
	{
		return Integer.toString(huisnummer).matches(HUISNUMMER_PATTERN);
	}
}
