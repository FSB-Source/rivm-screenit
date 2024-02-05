package nl.rivm.screenit.batch.jobs.colon.ifobtkoppelen;

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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseLogListener;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.IfobtKoppelingBeeindigdLogEvent;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.JobExecution;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class IfobtKoppelenListener extends BaseLogListener
{
	private final HibernateService hibernateService;

	@Override
	protected void beforeStarting(JobExecution jobExecution)
	{
		var koppelLogEvent = new IfobtKoppelingBeeindigdLogEvent();
		jobExecution.getExecutionContext().put(IfobtKoppelenConstants.RAPPORTAGEKEYIFOBTKOPPELEN, koppelLogEvent);
	}

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.IFOBT_KOPPELEN_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.IFOBT_KOPPELEN_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		String key = IfobtKoppelenConstants.RAPPORTAGEKEYIFOBTKOPPELEN;
		var context = getJobExecution().getExecutionContext();
		if (context.containsKey(key))
		{
			return (IfobtKoppelingBeeindigdLogEvent) context.get(key);
		}
		return null;
	}

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return Bevolkingsonderzoek.COLON;
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		var logEvent = getEindLogEvent();
		if (logEvent != null)
		{
			var koppelLogEvent = (IfobtKoppelingBeeindigdLogEvent) logEvent;
			if (!Level.ERROR.equals(koppelLogEvent.getLevel()) && jobHasExitCode(ExitStatus.FAILED))
			{
				String error = "De job heeft onsuccesvol gedraaid, neem contact op met de helpdesk";

				if (CollectionUtils.isNotEmpty(jobExecution.getAllFailureExceptions()))
				{
					Throwable exception = jobExecution.getAllFailureExceptions().get(0);
					error = exception.getMessage();
				}
				koppelLogEvent.setMelding(error);
				koppelLogEvent.setLevel(Level.ERROR);
			}
			else if (koppelLogEvent.getLevel() == null)
			{
				koppelLogEvent.setLevel(Level.INFO);
			}
			logEvent = koppelLogEvent;
			hibernateService.saveOrUpdate(koppelLogEvent);
		}
		else
		{
			logEvent = new LogEvent();
			logEvent.setLevel(Level.ERROR);
			logEvent.setMelding("LogEvent niet gevonden in de executionContext terwijl deze wel wordt verwacht");
		}
		return logEvent;
	}
}
