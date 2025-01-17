package nl.rivm.screenit.specification.algemeen;

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

import java.util.ArrayList;
import java.util.List;

import javax.persistence.metamodel.SingularAttribute;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.BagAdres_;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.colon.UitnodigingsGebied;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.topicuszorg.organisatie.model.Adres;
import nl.topicuszorg.organisatie.model.Adres_;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class BagAdresSpecification
{
	public static ExtendedSpecification<BagAdres> heeftGeenPostcodeCoordinaten()
	{
		return (r, q, cb) -> cb.isNull(r.get(BagAdres_.postcodeCoordinaten));

	}

	public static ExtendedSpecification<BagAdres> heeftGbaGemeente(Gemeente gbaGemeente)
	{
		return (r, q, cb) ->
			cb.equal(r.get(BagAdres_.gbaGemeente), gbaGemeente);
	}

	public static ExtendedSpecification<BagAdres> valtBinnen(UitnodigingsGebied uitnodigingsgebied)
	{
		if (uitnodigingsgebied == null)
		{
			return (r, q, cb) -> cb.conjunction();
		}

		var specification = heeftGbaGemeente(uitnodigingsgebied.getGemeente());

		if (uitnodigingsgebied.getPostcodeGebied() != null || uitnodigingsgebied.getGemeenteDeel() != null || uitnodigingsgebied.getWoonplaats() != null)
		{
			specification = specification.and(valtBinnenSpecifiekGebied(uitnodigingsgebied, false));
		}
		else if (uitnodigingsgebied.getGemeente() != null && uitnodigingsgebied.getGemeente().getUitnodigingsGebieden().size() > 1)
		{
			specification = specification.and(valtBinnenMeerdereUitnodigingsGebieden(uitnodigingsgebied));
		}

		return specification;
	}

	private static ExtendedSpecification<BagAdres> valtBinnenMeerdereUitnodigingsGebieden(UitnodigingsGebied uitnodigingsgebied)
	{
		var uitnodigingsGebieden = new ArrayList<>(uitnodigingsgebied.getGemeente().getUitnodigingsGebieden());
		uitnodigingsGebieden.remove(uitnodigingsgebied);

		ExtendedSpecification<BagAdres> excludes = (r, q, cb) -> cb.conjunction();
		for (UitnodigingsGebied uitnodigingsGebiedToExclude : uitnodigingsGebieden)
		{
			excludes = excludes.and(valtBinnenSpecifiekGebied(uitnodigingsGebiedToExclude, true));
		}

		var field = bepaalAdresField(uitnodigingsGebieden);

		if (field != null)
		{
			final var finalExcludes = excludes;
			return (r, q, cb) -> cb.or(cb.isNull(r.get(field)), finalExcludes.toPredicate(r, q, cb));
		}

		return (r, q, cb) -> cb.conjunction();
	}

	private static ExtendedSpecification<BagAdres> valtBinnenSpecifiekGebied(UitnodigingsGebied uitnodigingsgebied, boolean exclude)
	{
		return (r, q, cb) ->
		{
			if (uitnodigingsgebied.getPostcodeGebied() != null)
			{
				var postcodeRange = cb.and(cb.lessThanOrEqualTo(r.get(Adres_.postcode), uitnodigingsgebied.getPostcodeGebied().getTotPostcode()),
					cb.greaterThanOrEqualTo(r.get(Adres_.postcode), uitnodigingsgebied.getPostcodeGebied().getVanPostcode()));

				return exclude ? cb.not(postcodeRange) : postcodeRange;
			}

			if (uitnodigingsgebied.getGemeenteDeel() != null)
			{
				return exclude ?
					cb.notEqual(r.get(Adres_.gemeentedeel), uitnodigingsgebied.getGemeenteDeel()) :
					cb.equal(r.get(Adres_.gemeentedeel), uitnodigingsgebied.getGemeenteDeel());
			}

			if (uitnodigingsgebied.getWoonplaats() != null)
			{
				return exclude ?
					cb.notEqual(r.get(Adres_.plaats), uitnodigingsgebied.getWoonplaats()) :
					cb.equal(r.get(Adres_.plaats), uitnodigingsgebied.getWoonplaats());
			}

			return cb.conjunction();
		};
	}

	private static SingularAttribute<Adres, String> bepaalAdresField(List<UitnodigingsGebied> uitnodigingsGebieden)
	{
		SingularAttribute<Adres, String> field = null;
		if (uitnodigingsGebieden.get(0).getPostcodeGebied() != null)
		{
			field = Adres_.postcode;
		}
		else if (uitnodigingsGebieden.get(0).getGemeenteDeel() != null)
		{
			field = Adres_.gemeentedeel;
		}
		else if (uitnodigingsGebieden.get(0).getWoonplaats() != null)
		{
			field = Adres_.plaats;
		}
		return field;
	}
}
