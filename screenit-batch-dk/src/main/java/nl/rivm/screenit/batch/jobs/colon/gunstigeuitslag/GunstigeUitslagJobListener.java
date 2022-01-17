
package nl.rivm.screenit.batch.jobs.colon.gunstigeuitslag;

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
import nl.rivm.screenit.model.logging.GunstigeUitslagBeeindigdLogEvent;
import nl.rivm.screenit.model.logging.LogEvent;

import org.springframework.batch.core.JobExecution;
import org.springframework.batch.item.ExecutionContext;

public class GunstigeUitslagJobListener extends BaseLogListener
{

	@Override
	protected void beforeStarting(JobExecution jobExecution)
	{
		jobExecution.getExecutionContext().putInt(GunstigeUitslagConstants.GESELECTEERD, 0);
	}

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.GUNSTIGE_UITSLAG_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.GUNSTIGE_UITSLAG_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		return new GunstigeUitslagBeeindigdLogEvent();
	}

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return Bevolkingsonderzoek.COLON;
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		LogEvent logEvent = super.eindLogging(jobExecution);
		fillWaardes((GunstigeUitslagBeeindigdLogEvent) logEvent, jobExecution);
		return logEvent;
	}

	private void fillWaardes(GunstigeUitslagBeeindigdLogEvent log, JobExecution execution)
	{
		ExecutionContext context = execution.getExecutionContext();
		if (context.containsKey(GunstigeUitslagConstants.GESELECTEERD))
		{
			log.setAantalGunstigeBrievenUitGestuurd(context.getInt(GunstigeUitslagConstants.GESELECTEERD));
			if (!Level.ERROR.equals(log.getLevel()))
			{
				log.setLevel(Level.INFO);
				String melding = null;
				if (log.getAantalGunstigeBrievenUitGestuurd() == 0)
				{
					melding = "Er zijn geen clienten geselecteerd voor een gunstige uitslag brief.";
				}
				else if (log.getAantalGunstigeBrievenUitGestuurd() == 1)
				{
					melding = "Er is " + log.getAantalGunstigeBrievenUitGestuurd() + " gunstige brief uitgestuurd.";
				}
				else
				{
					melding = "Er zijn " + log.getAantalGunstigeBrievenUitGestuurd() + " gunstige brieven uitgestuurd.";
				}
				log.setMelding(melding);
			}
		}
	}
}
