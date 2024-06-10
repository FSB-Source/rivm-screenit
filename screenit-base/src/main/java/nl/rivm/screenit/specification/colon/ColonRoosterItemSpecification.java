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

import java.util.Date;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.rivm.screenit.model.colon.planning.RoosterItem_;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment_;

import org.springframework.data.jpa.domain.Specification;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class ColonRoosterItemSpecification
{
	public static Specification<RoosterItem> heeftKamer(Kamer locatie)
	{
		return (r, q, cb) -> cb.equal(r.get(AbstractAppointment_.location), locatie);
	}

	public static Specification<RoosterItem> heeftStartTijd(Date startTijd)
	{
		return (r, q, cb) -> cb.equal(r.get(AbstractAppointment_.startTime), startTijd);
	}

	public static Specification<RoosterItem> heeftGeenAfspraken()
	{
		return (r, q, cb) -> cb.isEmpty(r.get(RoosterItem_.afspraken));
	}
}
