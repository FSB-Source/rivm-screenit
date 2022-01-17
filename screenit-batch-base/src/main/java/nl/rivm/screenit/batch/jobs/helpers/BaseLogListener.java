
package nl.rivm.screenit.batch.jobs.helpers;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.lang.reflect.ParameterizedType;

import nl.rivm.screenit.batch.jobs.BatchConstants;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.JobType;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobExecutionListener;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Transactional(propagation = Propagation.REQUIRED)
public abstract class BaseLogListener implements JobExecutionListener
{

	@Autowired
	private LogService logService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	private JobExecution jobExecution = null;

	protected void beforeStarting(JobExecution jobExecution)
	{

	}

	@Override
	public void beforeJob(JobExecution jobExecution)
	{
		if (jobExecution != null)
		{
			this.jobExecution = jobExecution;
			jobExecution.setStartTime(currentDateSupplier.getDate());
		}
		beforeStarting(jobExecution);
		saveStartLogGebeurtenis();
	}

	protected void saveStartLogGebeurtenis()
	{
		LogGebeurtenis startLogGebeurtenis = getStartLogGebeurtenis();
		Bevolkingsonderzoek bevolkingsonderzoek = getBevolkingsonderzoek();
		LogEvent startLogEvent = getStartLogEvent();
		logService.logGebeurtenis(startLogGebeurtenis, startLogEvent, bevolkingsonderzoek);
	}

	protected abstract LogEvent getStartLogEvent();

	protected abstract LogGebeurtenis getStartLogGebeurtenis();

	@Override
	public void afterJob(JobExecution jobExecution)
	{
		if (jobExecution != null)
		{
			this.jobExecution = jobExecution;
		}
		beforeEindeLogging(jobExecution);
		LogEvent logEvent = eindLogging(jobExecution);
		saveEindLogGebeurtenis(logEvent);
	}

	protected void saveEindLogGebeurtenis(LogEvent logEvent)
	{
		Bevolkingsonderzoek bevolkingsonderzoek = getBevolkingsonderzoek();
		LogGebeurtenis eindLogGebeurtenis = getEindLogGebeurtenis();

		logService.logGebeurtenis(eindLogGebeurtenis, logEvent, bevolkingsonderzoek);
	}

	protected abstract LogGebeurtenis getEindLogGebeurtenis();

	protected abstract LogEvent getEindLogEvent();

	protected abstract Bevolkingsonderzoek getBevolkingsonderzoek();

	protected JobType getJobType()
	{
		return JobType.valueOf(jobExecution.getJobInstance().getJobName().toUpperCase());
	}

	protected void beforeEindeLogging(JobExecution jobExecution)
	{

	}

	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		LogEvent logEvent = getEindLogEvent();
		Level level = getLevel(jobExecution);
		addMelding(logEvent, getMelding(jobExecution));
		if (jobHasExitCode(ExitStatus.FAILED) || Level.ERROR.equals(level))
		{
			logEvent.setLevel(Level.ERROR);
			if (CollectionUtils.isNotEmpty(jobExecution.getAllFailureExceptions()))
			{
				addMelding(logEvent, "De job heeft onsuccesvol gedraaid, neem contact op met de helpdesk");
			}
		}
		else if (Level.WARNING.equals(level))
		{
			logEvent.setLevel(Level.WARNING);
		}
		else
		{
			logEvent.setLevel(Level.INFO);
		}
		return logEvent;
	}

	protected final static void addMelding(LogEvent logEvent, String melding)
	{
		String huidigeMelding = logEvent.getMelding();
		if (StringUtils.isBlank(huidigeMelding))
		{
			huidigeMelding = melding;

		}
		else if (!huidigeMelding.contains(melding))
		{
			huidigeMelding += "<br>" + melding;
		}
		logEvent.setMelding(huidigeMelding);
	}

	protected boolean jobHasExitCode(ExitStatus status)
	{
		return status != null && status.getExitCode() != null && jobExecution != null && jobExecution.getExitStatus() != null
			&& status.getExitCode().equals(jobExecution.getExitStatus().getExitCode());
	}

	protected String getStackTrace(Throwable aThrowable)
	{
		final Writer result = new StringWriter();
		final PrintWriter printWriter = new PrintWriter(result);
		aThrowable.printStackTrace(printWriter);
		if (result.toString().length() > 4000)
		{
			return result.toString().substring(0, 4000);
		}
		return result.toString();
	}

	protected Level getLevel(JobExecution execution)
	{
		Level level = Level.INFO;
		ExecutionContext context = execution.getExecutionContext();
		if (context.containsKey(BatchConstants.LEVEL))
		{
			level = (Level) context.get(BatchConstants.LEVEL);
		}
		return level;
	}

	protected String getMelding(JobExecution execution)
	{
		String melding = "";
		ExecutionContext context = execution.getExecutionContext();
		if (context.containsKey(BatchConstants.MELDING))
		{
			melding = context.getString(BatchConstants.MELDING);
		}
		return melding;
	}

	protected JobExecution getJobExecution()
	{
		return this.jobExecution;
	}

	protected <E extends Enum> void aantallenContextVerwerken(String enumKey, AantalVerwerker<E> aantalVerwerker)
	{
		ExecutionContext context = getJobExecution().getExecutionContext();

		E[] enumConstants = ((Class<E>) ((ParameterizedType) aantalVerwerker.getClass().getGenericSuperclass()).getActualTypeArguments()[0]).getEnumConstants();
		for (E enumConstant : enumConstants)
		{
			String key = BaseWriter.getEnumKey(enumKey, enumConstant);
			if (context.containsKey(key))
			{
				long aantal = context.getLong(key);
				aantalVerwerker.verwerk(enumConstant, aantal);
			}
		}
	}

	protected abstract class AantalVerwerker<E extends Enum>
	{

		protected abstract void verwerk(E enumConstant, long aantal);
	}
}
