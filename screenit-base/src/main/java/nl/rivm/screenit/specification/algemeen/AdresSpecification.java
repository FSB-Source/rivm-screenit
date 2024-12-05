package nl.rivm.screenit.specification.algemeen;

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

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.topicuszorg.organisatie.model.Adres;
import nl.topicuszorg.organisatie.model.Adres_;

import static nl.rivm.screenit.specification.SpecificationUtil.containsCaseInsensitive;
import static nl.rivm.screenit.specification.SpecificationUtil.isAttribuutGelijkOfNull;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmptyExtended;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNullExtended;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class AdresSpecification
{
	public static <T extends Adres> ExtendedSpecification<T> filterPostcode(String postcode)
	{
		return skipWhenEmptyExtended(postcode, (r, q, cb) -> cb.equal(r.get(Adres_.postcode), postcode));
	}

	public static <T extends Adres> ExtendedSpecification<T> filterPostcodeContaining(String postcode)
	{
		return skipWhenEmptyExtended(postcode, (r, q, cb) -> containsCaseInsensitive(cb, r.get(Adres_.postcode), postcode));
	}

	public static ExtendedSpecification<Adres> filterHuisnummer(Integer huisnummer)
	{
		return skipWhenNullExtended(huisnummer, (r, q, cb) -> cb.equal(r.get(Adres_.huisnummer), huisnummer));
	}

	public static ExtendedSpecification<Adres> heeftHuisnummer(Integer huisnummer)
	{
		return (r, q, cb) -> cb.equal(r.get(Adres_.huisnummer), huisnummer);
	}

	public static ExtendedSpecification<Adres> heeftPostcode(String postcode)
	{
		return (r, q, cb) -> cb.equal(r.get(Adres_.postcode), postcode);
	}

	public static ExtendedSpecification<Adres> heeftGeenPostcode()
	{
		return (r, q, cb) -> cb.isNull(r.get(Adres_.postcode));
	}

	public static ExtendedSpecification<Adres> heeftEenPostcode()
	{
		return (r, q, cb) -> cb.isNotNull(r.get(Adres_.postcode));
	}

	public static ExtendedSpecification<Adres> heeftHuisnummerOfNull(Integer huisnummer)
	{
		return isAttribuutGelijkOfNull(Adres_.huisnummer, huisnummer);
	}

	public static ExtendedSpecification<Adres> heeftHuisletterOfNull(String huisletter)
	{
		return isAttribuutGelijkOfNull(Adres_.huisletter, huisletter);
	}

	public static ExtendedSpecification<Adres> heeftHuisnummerToevoegingOfNull(String huisnummerToevoeging)
	{
		return isAttribuutGelijkOfNull(Adres_.huisnummerToevoeging, huisnummerToevoeging);
	}

	public static ExtendedSpecification<Adres> heeftHuisnummerAanduidingOfNull(String huisnummerAanduiding)
	{
		return isAttribuutGelijkOfNull(Adres_.huisnummerAanduiding, huisnummerAanduiding);
	}

	public static ExtendedSpecification<Adres> heeftPostcodeOfNull(String postcode)
	{
		return isAttribuutGelijkOfNull(Adres_.postcode, postcode);
	}

	public static ExtendedSpecification<Adres> heeftLocatieBeschrijvingOfNull(String locatieBeschrijving)
	{
		return isAttribuutGelijkOfNull(Adres_.locatieBeschrijving, locatieBeschrijving);
	}

	public static ExtendedSpecification<Adres> heeftAdresOfNull(Integer huisnummer, String huisletter, String huisnummerToevoeging, String huisnummerAanduiding, String postcode,
		String locatieBeschrijving)
	{
		return heeftHuisnummerOfNull(huisnummer)
			.and(heeftHuisletterOfNull(huisletter))
			.and(heeftHuisnummerToevoegingOfNull(huisnummerToevoeging))
			.and(heeftHuisnummerAanduidingOfNull(huisnummerAanduiding))
			.and(heeftPostcodeOfNull(postcode))
			.and(heeftLocatieBeschrijvingOfNull(locatieBeschrijving));
	}

	public static <T extends Adres> ExtendedSpecification<T> filterPlaatsContaining(String plaats)
	{
		return skipWhenEmptyExtended(plaats, (r, q, cb) -> containsCaseInsensitive(cb, r.get(Adres_.plaats), plaats));
	}
}
