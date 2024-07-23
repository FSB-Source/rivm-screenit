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

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.Instelling_;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.Kamer_;
import nl.rivm.screenit.model.colon.enums.ColonTijdSlotType;
import nl.rivm.screenit.specification.SpecificationUtil;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment_;
import nl.topicuszorg.wicket.planning.model.appointment.Location_;

import org.springframework.data.jpa.domain.Specification;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.DateSpecification.betweenDatesPredicate;

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
			var appointmentJoin = SpecificationUtil.join(subqueryRoot, Location_.appointments);

			subquery.select(subqueryRoot).where(
				cb.and(
				cb.equal(subqueryRoot.get(Kamer_.coloscopieCentrum), r),
					betweenDatesPredicate(bereik)
					.withPath(cb, appointmentJoin.get(AbstractAppointment_.startTime)),
					cb.equal(appointmentJoin.get(AbstractAppointment_.title), ColonTijdSlotType.ROOSTER_ITEM.getTitle()))
			);

			return cb.not(cb.exists(subquery));
		};
	}

	public static Specification<ColoscopieCentrum> isIntakelocatie(ColoscopieCentrum intakelocatie)
	{
		return (r, q, cb) -> cb.equal(r, intakelocatie);
	}
}
