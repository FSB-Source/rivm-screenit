package nl.rivm.screenit.batch.jobs.colon.oneindigetijdsloten;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment;
import nl.topicuszorg.wicket.planning.services.RecurrenceService;

import org.springframework.beans.factory.annotation.Autowired;

@Slf4j
public abstract class OneindigeTijdslotWriter<T extends AbstractAppointment> extends BaseWriter<T>
{

	@Autowired
	private RecurrenceService recurrenceService;

	@Override
	public void write(T item) throws Exception
	{
		if (LOG.isDebugEnabled())
		{
			String sb = "Uitrollen oneindig rooster, id: " + item.getId() + " (" + item.getClass().getSimpleName() + ")"
				+ ", type herhaling: " + item.getRecurrence().getClass().getSimpleName();

			LOG.debug(sb);
		}

		recurrenceService.toevoegenHerhaling(item, null);
	}

}
