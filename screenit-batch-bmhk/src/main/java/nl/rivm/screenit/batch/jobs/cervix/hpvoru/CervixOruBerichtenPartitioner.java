
package nl.rivm.screenit.batch.jobs.cervix.hpvoru;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import static nl.rivm.screenit.batch.jobs.cervix.hpvoru.CervixHpvOruBerichtenConstants.CERVIX_HPV_ORU_BERICHT_VERSTUURDEN_TIMEOUT;
import static nl.rivm.screenit.batch.jobs.cervix.hpvoru.CervixHpvOruBerichtenConstants.KEY_LABORATORIUMID;

import java.time.LocalDateTime;
import java.util.AbstractMap.SimpleEntry;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import nl.rivm.screenit.batch.jobs.helpers.BasePartitioner;
import nl.rivm.screenit.model.BMHKLaboratorium;

import org.hibernate.Criteria;
import org.hibernate.criterion.Restrictions;
import org.springframework.batch.item.ExecutionContext;

public class CervixOruBerichtenPartitioner extends BasePartitioner
{

	private int timeout = -1;

	private int getTimeout()
	{

		return timeout;
	}

	public void setTimeout(int timeout)
	{
		this.timeout = timeout;
	}

	private List<BMHKLaboratorium> getActieveBMHKLaboratoria()
	{
		Criteria crit = getHibernateSession().createCriteria(BMHKLaboratorium.class);
		crit.add(Restrictions.eq("actief", true));
		return crit.list();
	}

	@Override
	public Map<String, ExecutionContext> setPartition(int gridSize)
	{
		if (getTimeout() == -1)
		{
			throw new IllegalStateException("Timeout moet gezet worden voor deze job.");
		}
		LocalDateTime timeoutTime = LocalDateTime.now().plusMinutes(getTimeout());

		return getActieveBMHKLaboratoria().stream()
			.map(
				lab -> new SimpleEntry<>(
					String.valueOf(lab.getId()),
					new ExecutionContext(
						Stream.of(
							new SimpleEntry<>(KEY_LABORATORIUMID, lab.getId()),
							new SimpleEntry<>(CERVIX_HPV_ORU_BERICHT_VERSTUURDEN_TIMEOUT, timeoutTime))
							.collect(
								Collectors.toMap(SimpleEntry::getKey, SimpleEntry::getValue)))))
			.collect(Collectors.toMap(SimpleEntry::getKey, SimpleEntry::getValue));
	}

}
