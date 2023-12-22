package nl.rivm.screenit.specification.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.Instelling_;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.Kamer_;
import nl.rivm.screenit.specification.SpecificationUtil;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment_;
import nl.topicuszorg.wicket.planning.model.appointment.Location_;

import org.springframework.data.jpa.domain.Specification;

import com.google.common.collect.Range;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ColonIntakelocatieSpecification
{
	public static Specification<ColoscopieCentrum> isActief()
	{
		return (r, q, cb) -> cb.isTrue(r.get(Instelling_.actief));
	}

	public static Specification<ColoscopieCentrum> heeftGeenCapaciteitBinnenDatum(Range<LocalDate> bereik)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Kamer.class);
			var subqueryRoot = subquery.from(Kamer.class);

			subquery.select(subqueryRoot).where(
				cb.equal(subqueryRoot.get(Kamer_.coloscopieCentrum), r),
				SpecificationUtil.betweenDatesPredicate(bereik)
					.withPath(cb, SpecificationUtil.join(subqueryRoot, Location_.appointments).get(AbstractAppointment_.startTime))
			);

			return cb.not(cb.exists(subquery));
		};
	}
}
