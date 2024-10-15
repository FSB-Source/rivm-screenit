package nl.rivm.screenit.specification.colon;

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
import java.time.LocalDateTime;

import javax.persistence.criteria.JoinType;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.colon.planning.ColonAfspraakslot;
import nl.rivm.screenit.model.colon.planning.ColonAfspraakslot_;
import nl.rivm.screenit.model.colon.planning.ColonIntakekamer;
import nl.rivm.screenit.model.colon.planning.ColonTijdslot_;
import nl.rivm.screenit.specification.SpecificationUtil;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.domain.Specification;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.DateSpecification.overlaptLocalDate;
import static nl.rivm.screenit.specification.RangeSpecification.overlapt;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class ColonAfspraakslotSpecification
{
	public static Specification<ColonAfspraakslot> heeftId(Long id)
	{
		return SpecificationUtil.skipWhenNull(id, (r, q, cb) -> cb.equal(r.get(ColonTijdslot_.id), id));
	}

	public static Specification<ColonAfspraakslot> valtBinnenDatumTijdRange(Range<LocalDateTime> range)
	{
		return overlapt(range, r -> r.get(ColonTijdslot_.vanaf), r -> r.get(ColonTijdslot_.tot));
	}

	public static Specification<ColonAfspraakslot> valtBinnenDatumRange(Range<LocalDate> range)
	{
		return overlaptLocalDate(range, r -> r.get(ColonTijdslot_.vanaf), r -> r.get(ColonTijdslot_.tot));
	}

	public static Specification<ColonAfspraakslot> heeftKamer(ColonIntakekamer kamer)
	{
		return (r, q, cb) -> cb.equal(r.get(ColonTijdslot_.kamer), kamer);
	}

	public static Specification<ColonAfspraakslot> heeftVanaf(LocalDateTime vanaf)
	{
		return (r, q, cb) -> cb.equal(r.get(ColonTijdslot_.vanaf), vanaf);
	}

	public static Specification<ColonAfspraakslot> heeftGeenAfspraak()
	{
		return (r, q, cb) ->
		{
			var afspraakJoin = r.join(ColonAfspraakslot_.afspraak, JoinType.LEFT);
			return cb.isNull(afspraakJoin.get(AbstractHibernateObject_.id));
		};
	}
}
