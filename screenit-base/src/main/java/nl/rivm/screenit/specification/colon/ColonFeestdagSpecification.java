package nl.rivm.screenit.specification.colon;

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

import java.time.LocalDate;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.colon.ColonFeestdag;
import nl.rivm.screenit.model.colon.ColonFeestdag_;
import nl.rivm.screenit.model.colon.dto.ColonFeestdagDto;
import nl.rivm.screenit.specification.RangeSpecification;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.domain.Specification;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNull;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ColonFeestdagSpecification
{
	public static Specification<ColonFeestdag> isActief()
	{
		return ((r, q, cb) -> cb.isTrue(r.get(ColonFeestdag_.actief)));
	}

	public static Specification<ColonFeestdag> heeftDatumVanaf(LocalDate datum)
	{
		return ((r, q, cb) -> cb.greaterThanOrEqualTo(r.get(ColonFeestdag_.datum), datum));
	}

	public static Specification<ColonFeestdag> heeftDatumInRange(LocalDate vanaf, LocalDate totEnMet)
	{
		var range = Range.closed(vanaf, totEnMet);
		return RangeSpecification.bevat(range, r -> r.get(ColonFeestdag_.datum));
	}

	public static Specification<ColonFeestdag> isNietFeestdag(ColonFeestdagDto feestdagDto)
	{
		return skipWhenNull(feestdagDto.getId(), (r, q, cb) -> cb.notEqual(r.get(AbstractHibernateObject_.id), feestdagDto.getId()));
	}
}
