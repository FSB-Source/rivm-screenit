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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.criteria.Predicate;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.planning.ColonBlokkade;
import nl.rivm.screenit.specification.SpecificationUtil;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment_;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.NoRecurrence;

import org.springframework.data.jpa.domain.Specification;

import com.google.common.collect.Range;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class ColonBlokkadeSpecification
{
	public static Specification<ColonBlokkade> heeftKamer(Kamer kamer)
	{
		return (r, q, cb) ->
			cb.equal(cb.treat(r.get(AbstractAppointment_.location), Kamer.class), kamer);
	}

	public static Specification<ColonBlokkade> valtBinnenRanges(List<Range<Date>> ranges)
	{
		return (r, q, cb) ->
		{
			var predicates = new ArrayList<Predicate>();
			for (var range : ranges)
			{
				predicates.add(valtBinnenDatumRange(range).toPredicate(r, q, cb));
			}
			return SpecificationUtil.composePredicatesOr(cb, predicates);
		};
	}

	public static Specification<ColonBlokkade> valtBinnenDatumRange(Range<Date> range)
	{
		return (r, q, cb) ->
		{
			var startProperty = r.get(AbstractAppointment_.startTime);
			var endProperty = r.get(AbstractAppointment_.endTime);
			return cb.and(cb.greaterThan(endProperty, range.lowerEndpoint()), cb.lessThan(startProperty, range.upperEndpoint())
			);
		};
	}

	public static Specification<ColonBlokkade> valtBinnenRecurrence(ColonBlokkade blokkade, Range<Date> totaalInterval)
	{
		return (r, q, cb) ->
		{
			var recurrence = blokkade.getRecurrence();
			if (recurrence != null && recurrence.getId() != null && !NoRecurrence.class.isAssignableFrom(recurrence.getClass()))
			{
				var recurrencePredicates = new ArrayList<Predicate>();
				recurrencePredicates.add(cb.equal(r.get(AbstractAppointment_.recurrence), recurrence));
				if (totaalInterval != null)
				{
					recurrencePredicates.add(cb.greaterThanOrEqualTo(r.get(AbstractAppointment_.startTime), totaalInterval.lowerEndpoint()));
					recurrencePredicates.add(cb.lessThan(r.get(AbstractAppointment_.endTime), totaalInterval.upperEndpoint()));
				}
				return SpecificationUtil.composePredicates(cb, recurrencePredicates);
			}
			else if (blokkade.getId() != null)
			{
				return cb.notEqual(r, blokkade);
			}
			return null;
		};
	}
}
