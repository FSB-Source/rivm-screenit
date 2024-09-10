package nl.rivm.screenit.specification.mamma;

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

import java.time.LocalDate;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaPostcodeReeks_;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode_;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde_;
import nl.rivm.screenit.model.mamma.MammaStandplaats_;
import nl.topicuszorg.organisatie.model.Adres_;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.util.Pair;

import static com.google.common.collect.BoundType.CLOSED;
import static nl.rivm.screenit.specification.RangeSpecification.bevat;
import static nl.rivm.screenit.specification.SpecificationUtil.containsCaseInsensitive;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmpty;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNull;
import static nl.rivm.screenit.util.DateUtil.toUtilDate;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaStandplaatsSpecification
{
	public static Specification<MammaStandplaatsPeriode> heeftStandplaatsOpOfNaDatum(MammaStandplaats standplaats, LocalDate afsprakenVanafDatum)
	{
		return (r, q, cb) ->
		{
			var rondeJoin = join(r, MammaStandplaatsPeriode_.standplaatsRonde);
			return cb.and(
				cb.equal(rondeJoin.get(MammaStandplaatsRonde_.standplaats), standplaats),
				cb.greaterThanOrEqualTo(r.get(MammaStandplaatsPeriode_.totEnMet), toUtilDate(afsprakenVanafDatum))
			);
		};
	}

	public static Specification<MammaStandplaats> filterOpNaam(String naam)
	{
		return skipWhenEmpty(naam, (r, q, cb) -> containsCaseInsensitive(cb, r.get(MammaStandplaats_.naam), naam));
	}

	public static Specification<MammaStandplaats> filterOpLocatie(String plaats)
	{
		return skipWhenEmpty(plaats, (r, q, cb) ->
		{
			var locatieJoin = join(r, MammaStandplaats_.locatie);
			var tijdelijkeLocatieJoin = join(r, MammaStandplaats_.tijdelijkeLocatie);

			return cb.or(
				containsCaseInsensitive(cb, locatieJoin.get(Adres_.plaats), plaats),
				containsCaseInsensitive(cb, tijdelijkeLocatieJoin.get(Adres_.plaats), plaats)
			);
		});
	}

	public static Specification<MammaStandplaats> filterOpRegio(ScreeningOrganisatie screeningOrganisatie)
	{
		return skipWhenNull(screeningOrganisatie, (r, q, cb) -> cb.equal(r.get(MammaStandplaats_.regio), screeningOrganisatie));
	}

	public static Specification<MammaStandplaats> filterOpActief(Boolean actief)
	{
		return skipWhenNull(actief, (r, q, cb) -> cb.equal(r.get(MammaStandplaats_.ACTIEF), actief));
	}

	public static Specification<MammaStandplaats> heeftPostcode(String postcode)
	{
		return (r, q, cb) ->
		{
			var postcodeReeksJoin = join(r, MammaStandplaats_.postcodeReeksen);
			return cb.and(
				bevat(postcodeReeksJoin.get(MammaPostcodeReeks_.vanPostcode), postcodeReeksJoin.get(MammaPostcodeReeks_.totPostcode), Pair.of(CLOSED, CLOSED), postcode)
					.withPath(cb, r),
				cb.equal(r.get(MammaStandplaats_.ACTIEF), true)
			);
		};
	}
}
