
package nl.rivm.screenit.batch.jobs.colon.oneindigetijdsloten;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment;
import nl.topicuszorg.wicket.planning.services.RecurrenceService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

public abstract class OneindigeTijdslotWriter<T extends AbstractAppointment> extends BaseWriter<T>
{

	private static final Logger LOG = LoggerFactory.getLogger(OneindigeTijdslotWriter.class);

	@Autowired
	private RecurrenceService recurrenceService;

	@Override
	public void write(T item) throws Exception
	{
		if (LOG.isDebugEnabled())
		{
			StringBuilder sb = new StringBuilder();

			sb.append("Uitrollen oneindig rooster, id: ").append(item.getId()).append(" (").append(item.getClass().getSimpleName()).append(")");
			sb.append(", type herhaling: ").append(item.getRecurrence().getClass().getSimpleName());

			LOG.debug(sb.toString());
		}

		recurrenceService.toevoegenHerhaling(item, null);
	}

}
