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

import nl.rivm.screenit.model.PostcodeCoordinaten;
import nl.rivm.screenit.model.PostcodeCoordinaten_;

import org.apache.commons.lang.StringUtils;
import org.springframework.data.jpa.domain.Specification;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class PostcodeCoordinatenSpecification
{
	public static Specification<PostcodeCoordinaten> heeftPostcode(String postcode)
	{
		return (r, q, cb) -> cb.equal(r.get(PostcodeCoordinaten_.postcode), StringUtils.deleteWhitespace(postcode));
	}

	public static Specification<PostcodeCoordinaten> heeftHuisnummer(Integer huisnummer)
	{
		return (r, q, cb) -> cb.equal(r.get(PostcodeCoordinaten_.huisnummer), huisnummer);
	}

	public static Specification<PostcodeCoordinaten> heeftGeenHuisnummerToevoeging()
	{
		return (r, q, cb) -> cb.isNull(r.get(PostcodeCoordinaten_.huisnummerToevoeging));
	}

	public static Specification<PostcodeCoordinaten> heeftHuisnummerToevoeging(String huisnummerToevoeging)
	{
		return (r, q, cb) -> cb.equal(r.get(PostcodeCoordinaten_.huisnummerToevoeging), huisnummerToevoeging);
	}
}
