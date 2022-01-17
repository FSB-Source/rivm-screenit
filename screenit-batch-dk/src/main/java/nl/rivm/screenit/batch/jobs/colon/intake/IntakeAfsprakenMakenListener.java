package nl.rivm.screenit.batch.jobs.colon.intake;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import nl.rivm.screenit.batch.jobs.helpers.BaseLogListener;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.IntakeMakenLogEvent;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.topicuszorg.util.date.DateUtil;

import org.apache.commons.lang3.StringUtils;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.item.ExecutionContext;

public class IntakeAfsprakenMakenListener extends BaseLogListener
{

	@Override
	protected void beforeStarting(JobExecution jobExecution)
	{
		IntakeMakenLogEvent intakeMakenLogEvent = new IntakeMakenLogEvent();
		jobExecution.getExecutionContext().put(IntakeAfsprakenMakenConstants.RAPPORTAGEKEYINTAKE, intakeMakenLogEvent);
	}

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.INTAKE_AFSPRAAK_MAKEN_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.INTAKE_AFSPRAAK_MAKEN_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		String key = IntakeAfsprakenMakenConstants.RAPPORTAGEKEYINTAKE;
		ExecutionContext executionContext = this.getJobExecution().getExecutionContext();
		if (executionContext.containsKey(key))
		{
			return (IntakeMakenLogEvent) getJobExecution().getExecutionContext().get(key);
		}
		return null;
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		LogEvent logEvent = getEindLogEvent();
		if (logEvent instanceof IntakeMakenLogEvent)
		{
			IntakeMakenLogEvent intakeMakenLogEvent = (IntakeMakenLogEvent) logEvent;

			for (Throwable throwable : jobExecution.getAllFailureExceptions())
			{
				intakeMakenLogEvent.getExceptionStackTrace().add(getStackTrace(throwable));
				intakeMakenLogEvent.setLevel(Level.ERROR);
			}
			int gebruikteDagen = 0;
			if (intakeMakenLogEvent.getBeginTijd() != null && intakeMakenLogEvent.getEindTijd() != null)
			{
				gebruikteDagen = DateUtil.aantalDagenVerschil(intakeMakenLogEvent.getBeginTijd(), intakeMakenLogEvent.getEindTijd());
			}
			String melding = String.format(
				"#%s clienten geselecteerd, #%s vrije sloten, #%s gebruikte dagen, #%s extra dagen, #%s extra pogingen, #%s boven max. afstand geplaatst",
				intakeMakenLogEvent.getAantalClienten(), intakeMakenLogEvent.getAantalVrijesloten(), gebruikteDagen, intakeMakenLogEvent.getAantalExtraDagen(),
				intakeMakenLogEvent.getAantalRondes(), intakeMakenLogEvent.getAantalBuitenMaximaleAfstand());
			String foutMelding = null;
			if (jobExecution.getExecutionContext().containsKey(IntakeAfsprakenMakenConstants.FOUT_BIJ_INTAKE_VASTLEGGEN))
			{
				foutMelding = jobExecution.getExecutionContext().getString(IntakeAfsprakenMakenConstants.FOUT_BIJ_INTAKE_VASTLEGGEN);
				if (StringUtils.isNotBlank(foutMelding))
				{
					melding += "<br>" + foutMelding;
					if (intakeMakenLogEvent.getLevel() != Level.ERROR)
					{
						intakeMakenLogEvent.setLevel(Level.WARNING);
					}
				}
			}
			intakeMakenLogEvent.setMelding(melding);
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
		return Bevolkingsonderzoek.COLON;
	}
}
