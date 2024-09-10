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

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.rivm.screenit.specification.SpecificationUtil;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment_;

import org.springframework.data.jpa.domain.Specification;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.DateSpecification.overlaptLocalDate;
import static nl.rivm.screenit.specification.DateSpecification.overlaptLocalDateTime;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class ColonAfspraakslotSpecification
{
	public static Specification<RoosterItem> heeftKamer(Kamer kamer)
	{
		return (r, q, cb) ->
			cb.equal(cb.treat(r.get(AbstractAppointment_.location), Kamer.class), kamer);
	}

	public static Specification<RoosterItem> heeftId(Long id)
	{
		return SpecificationUtil.skipWhenNull(id, (r, q, cb) -> cb.equal(r.get(AbstractAppointment_.id), id));
	}

	public static Specification<RoosterItem> valtBinnenDatumTijdRange(Range<LocalDateTime> range)
	{
		return overlaptLocalDateTime(range, r -> r.get(AbstractAppointment_.startTime), r -> r.get(AbstractAppointment_.endTime));
	}

	public static Specification<RoosterItem> valtBinnenDatumRange(Range<LocalDate> range)
	{
		return overlaptLocalDate(range, r -> r.get(AbstractAppointment_.startTime), r -> r.get(AbstractAppointment_.endTime));
	}
}
