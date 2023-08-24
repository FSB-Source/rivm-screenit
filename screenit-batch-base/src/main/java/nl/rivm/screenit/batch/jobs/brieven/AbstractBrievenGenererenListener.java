package nl.rivm.screenit.batch.jobs.brieven;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import nl.rivm.screenit.batch.jobs.helpers.BaseLogListener;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.BrievenGenererenBeeindigdLogEvent;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.verwerkingverslag.BrievenGenererenRapportage;
import nl.rivm.screenit.model.verwerkingverslag.BrievenGenererenRapportageEntry;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.batch.core.JobExecution;
import org.springframework.beans.factory.annotation.Autowired;

public abstract class AbstractBrievenGenererenListener extends BaseLogListener
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	protected void beforeStarting(JobExecution jobExecution)
	{
		var map = new HashMap<Long, Integer>();
		List<ScreeningOrganisatie> screeningOrganisaties = hibernateService.loadAll(ScreeningOrganisatie.class);
		for (ScreeningOrganisatie org : screeningOrganisaties)
		{
			map.put(org.getId(), 0);
		}
		jobExecution.getExecutionContext().put(getRapportageAantalBrievenKey(), map);
	}

	protected abstract String getRapportageAantalBrievenKey();

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected abstract LogGebeurtenis getStartLogGebeurtenis();

	@Override
	protected abstract LogGebeurtenis getEindLogGebeurtenis();

	@Override
	protected LogEvent getEindLogEvent()
	{
		return new BrievenGenererenBeeindigdLogEvent();
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		BrievenGenererenBeeindigdLogEvent event = (BrievenGenererenBeeindigdLogEvent) super.eindLogging(jobExecution);

		BrievenGenererenRapportage rapportage = new BrievenGenererenRapportage();
		rapportage.setDatumVerwerking(currentDateSupplier.getDate());
		event.setRapportage(rapportage);
		var map = (Map<Long, Integer>) jobExecution.getExecutionContext().get(getRapportageAantalBrievenKey());

		if (map != null)
		{
			for (Entry<Long, Integer> entry : map.entrySet())
			{
				BrievenGenererenRapportageEntry rapportageEntry = new BrievenGenererenRapportageEntry();
				rapportageEntry.setScreeningOrganisatie(hibernateService.get(ScreeningOrganisatie.class, entry.getKey()));
				rapportageEntry.setRapportage(rapportage);
				rapportageEntry.setAantalBrievenPerScreeningOrganisatie(entry.getValue());
				rapportage.getEntries().add(rapportageEntry);
				hibernateService.saveOrUpdate(rapportageEntry);
			}
			hibernateService.saveOrUpdate(rapportage);
		}
		hibernateService.saveOrUpdate(event);

		return event;
	}

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return null;
	}

}
