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

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.PostcodeGebied;
import nl.rivm.screenit.model.PostcodeGebied_;
import nl.rivm.screenit.specification.SpecificationUtil;

import org.springframework.data.jpa.domain.Specification;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class PostcodeGebiedSpecification
{
	public static Specification<PostcodeGebied> isAnderPostcodeGebied(PostcodeGebied postcodeGebied)
	{
		return SpecificationUtil.skipWhenNull(postcodeGebied.getId(), (r, q, cb) -> cb.notEqual(r, postcodeGebied));
	}

	public static Specification<PostcodeGebied> heeftOverlappendePostcode(PostcodeGebied postcodeGebied)
	{
		var vanPostcode = postcodeGebied.getVanPostcode().toUpperCase();
		var totPostcode = postcodeGebied.getTotPostcode().toUpperCase();

		return (r, q, cb) ->
			cb.or(
				postcodeValtInRange(r, cb, vanPostcode),
				postcodeValtInRange(r, cb, totPostcode),
				cb.and(
					cb.greaterThanOrEqualTo(r.get(PostcodeGebied_.vanPostcode), vanPostcode),
					cb.lessThanOrEqualTo(r.get(PostcodeGebied_.totPostcode), totPostcode)
				)
			);
	}

	private static Predicate postcodeValtInRange(Root<PostcodeGebied> r, CriteriaBuilder cb, String postcode)
	{
		return cb.and(
			cb.lessThanOrEqualTo(r.get(PostcodeGebied_.vanPostcode), postcode),
			cb.greaterThanOrEqualTo(r.get(PostcodeGebied_.totPostcode), postcode)
		);
	}
}
