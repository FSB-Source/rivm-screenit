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

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.EnovationHuisarts_;
import nl.rivm.screenit.model.Huisarts_;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.topicuszorg.organisatie.model.Adres;
import nl.topicuszorg.organisatie.model.Adres_;

import org.apache.commons.lang.StringUtils;
import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.composePredicates;
import static nl.rivm.screenit.specification.SpecificationUtil.containsCaseInsensitive;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenBlankPredicate;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmpty;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNull;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class EnovationHuisartsSpecification
{

	public static ExtendedSpecification<EnovationHuisarts> isVerwijderd(boolean verwijderd)
	{
		return (r, q, cb) -> cb.equal(r.get(Huisarts_.verwijderd), verwijderd);
	}

	public static Specification<EnovationHuisarts> filterNaam(String naam)
	{
		return skipWhenEmpty(naam, (r, q, cb) -> cb.or(
			containsCaseInsensitive(cb, r.get(EnovationHuisarts_.achternaam), naam),
			containsCaseInsensitive(cb, r.get(EnovationHuisarts_.weergavenaam), naam),
			containsCaseInsensitive(cb, r.get(EnovationHuisarts_.praktijknaam), naam)));
	}

	public static Specification<EnovationHuisarts> filterAdres(Adres zoekAdres)
	{
		return skipWhenNull(zoekAdres, (r, q, cb) ->
		{
			var adresJoin = join(r, EnovationHuisarts_.adres);
			var postcode = StringUtils.deleteWhitespace(zoekAdres.getPostcode());
			return composePredicates(cb,
				skipWhenBlankPredicate(postcode, containsCaseInsensitive(cb, adresJoin.get(Adres_.postcode), postcode)),
				skipWhenBlankPredicate(zoekAdres.getPlaats(), containsCaseInsensitive(cb, adresJoin.get(Adres_.plaats), zoekAdres.getPlaats())),
				skipWhenBlankPredicate(zoekAdres.getStraat(), containsCaseInsensitive(cb, adresJoin.get(Adres_.straat), zoekAdres.getStraat())));
		});
	}
}
