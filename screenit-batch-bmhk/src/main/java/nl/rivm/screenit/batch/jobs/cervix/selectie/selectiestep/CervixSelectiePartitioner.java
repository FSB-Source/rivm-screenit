package nl.rivm.screenit.batch.jobs.cervix.selectie.selectiestep;

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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.batch.jobs.helpers.BasePartitioner;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.service.InstellingService;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5Session;

import org.springframework.batch.item.ExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;

public class CervixSelectiePartitioner extends BasePartitioner
{

	public final static String KEY_BMHK_LAB = "bmhk_lab";

	@Autowired
	private InstellingService instellingService;

	@Override
	public Map<String, ExecutionContext> setPartition(int gridSize)
	{
		Map<String, ExecutionContext> partities = new HashMap<String, ExecutionContext>(gridSize);
		OpenHibernate5Session.withoutTransaction().run(() -> {
			List<BMHKLaboratorium> bmhkLabs = instellingService.getActieveInstellingen(BMHKLaboratorium.class);

			for (BMHKLaboratorium bmhkLab : bmhkLabs)
			{
				ExecutionContext executionContext = new ExecutionContext();
				executionContext.put(KEY_BMHK_LAB, bmhkLab.getId());
				partities.put("BMHK Lab" + bmhkLab.getId(), executionContext);
			}
		});
		return partities;
	}

}
