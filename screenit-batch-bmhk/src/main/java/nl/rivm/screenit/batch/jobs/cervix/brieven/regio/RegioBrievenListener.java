package nl.rivm.screenit.batch.jobs.cervix.brieven.regio;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Map;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.BatchConstants;
import nl.rivm.screenit.batch.jobs.helpers.BaseLogListener;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.BrievenGenererenBeeindigdLogEvent;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.verwerkingverslag.BrievenGenererenRapportage;
import nl.rivm.screenit.model.verwerkingverslag.BrievenGenererenRapportageEntry;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.JobExecution;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class RegioBrievenListener extends BaseLogListener
{

	private final HibernateService hibernateService;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	protected void beforeStarting(JobExecution jobExecution)
	{
		var brievenLogEvent = new BrievenGenererenBeeindigdLogEvent();
		Map<Long, Integer> map = new HashMap<>();

		var screeningOrganisaties = hibernateService.loadAll(ScreeningOrganisatie.class);
		for (var screeningOrganisatie : screeningOrganisaties)
		{
			map.put(screeningOrganisatie.getId(), 0);
		}
		hibernateService.saveOrUpdate(brievenLogEvent);
		jobExecution.getExecutionContext().put(RegioBrievenConstants.RAPPORTAGEKEYBRIEVEN, brievenLogEvent.getId());
		jobExecution.getExecutionContext().put(RegioBrievenConstants.RAPPORTAGEKEYAANTALBRIEVEN, map);
	}

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.REGIO_BRIEVEN_GENERENEN_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.REGIO_BRIEVEN_GENERENEN_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		var key = RegioBrievenConstants.RAPPORTAGEKEYBRIEVEN;
		var executionContext = getJobExecution().getExecutionContext();
		if (executionContext.containsKey(key))
		{
			var brievenlogEventid = (Long) getJobExecution().getExecutionContext().get(key);
			return hibernateService.load(BrievenGenererenBeeindigdLogEvent.class, brievenlogEventid);
		}
		return null;
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		var logEvent = getEindLogEvent();
		if (logEvent != null)
		{
			var brievenLogEvent = (BrievenGenererenBeeindigdLogEvent) logEvent;
			if (jobHasExitCode(ExitStatus.FAILED))
			{
				var error = "De job heeft onsuccesvol gedraaid, neem contact op met de helpdesk";
				var context = jobExecution.getExecutionContext();

				if (context.containsKey(BatchConstants.MELDING) && context.get(BatchConstants.MELDING) == null)
				{
					error = (String) context.get(BatchConstants.MELDING);
				}
				brievenLogEvent.setMelding(error);
				brievenLogEvent.setLevel(Level.ERROR);
			}
			else
			{
				brievenLogEvent.setLevel(Level.INFO);
			}

			var rapportage = new BrievenGenererenRapportage();
			rapportage.setDatumVerwerking(currentDateSupplier.getDate());
			brievenLogEvent.setRapportage(rapportage);
			Map<Long, Integer> map = (Map<Long, Integer>) jobExecution.getExecutionContext().get(RegioBrievenConstants.RAPPORTAGEKEYAANTALBRIEVEN);

			for (var entry : map.entrySet())
			{
				var rapportageEntry = new BrievenGenererenRapportageEntry();
				rapportageEntry.setScreeningOrganisatie(hibernateService.load(ScreeningOrganisatie.class, entry.getKey()));
				rapportageEntry.setRapportage(rapportage);
				rapportageEntry.setAantalBrievenPerScreeningOrganisatie(entry.getValue());
				rapportage.getEntries().add(rapportageEntry);
				hibernateService.saveOrUpdate(rapportageEntry);
			}
			hibernateService.saveOrUpdate(rapportage);

			for (var entry : brievenLogEvent.getRapportage().getEntries())
			{
				hibernateService.saveOrUpdate(entry);
			}
			hibernateService.saveOrUpdate(brievenLogEvent);
			logEvent = brievenLogEvent;

		}
		else
		{
			logEvent = new LogEvent();
			logEvent.setLevel(Level.ERROR);
			logEvent.setMelding("LogEvent niet gevonden in de executionContext terwijl deze wel wordt verwacht");
		}
		return logEvent;
	}

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return null;
	}
}
