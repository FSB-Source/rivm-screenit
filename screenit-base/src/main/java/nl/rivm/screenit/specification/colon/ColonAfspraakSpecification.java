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

import java.util.List;

import javax.persistence.criteria.JoinType;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.Afspraak;
import nl.rivm.screenit.model.Afspraak_;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.rivm.screenit.specification.SpecificationUtil;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment_;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.AbstractRecurrence_;

import org.springframework.data.jpa.domain.Specification;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class ColonAfspraakSpecification
{
	public static Specification<Afspraak> heeftKamer(Kamer locatie)
	{
		return (r, q, cb) -> cb.equal(r.get(AbstractAppointment_.location), locatie);
	}

	public static Specification<Afspraak> heeftStatuses(List<AfspraakStatus> statuses)
	{
		return (r, q, cb) -> r.get(Afspraak_.status).in(statuses);
	}

	public static Specification<Afspraak> heeftRoosterItemRecurrence(Long roosterItemId)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var subqueryRoot = subquery.from(RoosterItem.class);
			var recurrenceJoin = SpecificationUtil.join(subqueryRoot, AbstractAppointment_.recurrence);
			var appointmentsJoin = SpecificationUtil.join(recurrenceJoin, AbstractRecurrence_.appointments, JoinType.LEFT);
			var roosterItem = SpecificationUtil.join(r, Afspraak_.roosterItem);
			subquery.select(subqueryRoot.get(AbstractAppointment_.id)).where(cb.equal(appointmentsJoin.get(AbstractAppointment_.id), roosterItemId));

			return cb.or(cb.equal(roosterItem.get(AbstractAppointment_.id), roosterItemId), cb.in(roosterItem.get(AbstractAppointment_.id)).value(subquery));
		};
	}
}
