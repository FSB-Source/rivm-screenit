package nl.rivm.screenit.batch.jobs.colon.ifobtrappel;

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
import nl.rivm.screenit.model.logging.IfobtHerinneringBeeindigdLogEvent;
import nl.rivm.screenit.model.logging.LogEvent;

import org.springframework.batch.core.JobExecution;
import org.springframework.stereotype.Component;

@Component
public class IfobtRappelJobListener extends BaseLogListener
{

	@Override
	protected void beforeStarting(JobExecution jobExecution)
	{
		jobExecution.getExecutionContext().putInt(IfobtRappelJobConstants.HERINNERD, 0);
		jobExecution.getExecutionContext().putInt(IfobtRappelJobConstants.GESELECTEERD, 0);
	}

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.IFOBT_HERINNERING_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.IFOBT_HERINNERING_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		return new IfobtHerinneringBeeindigdLogEvent();
	}

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return Bevolkingsonderzoek.COLON;
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		var logEvent = super.eindLogging(jobExecution);
		var context = jobExecution.getExecutionContext();
		var herinneringLogEvent = (IfobtHerinneringBeeindigdLogEvent) logEvent;
		herinneringLogEvent.setAantalGeselecteerd(context.getInt(IfobtRappelJobConstants.GESELECTEERD));
		herinneringLogEvent.setAantalHerinnering(context.getInt(IfobtRappelJobConstants.HERINNERD));

		if (herinneringLogEvent.getAantalGeselecteerd() == 0 && !Level.ERROR.equals(herinneringLogEvent.getLevel()))
		{
			herinneringLogEvent.setMelding("Er zijn geen clienten geselecteerd voor een herinneringsbrief.");
			herinneringLogEvent.setLevel(Level.WARNING);
		}
		else if (!Level.ERROR.equals(herinneringLogEvent.getLevel()))
		{
			herinneringLogEvent.setLevel(Level.INFO);
			String melding = "Aantal herinneringsbrieven gemaakt: " + herinneringLogEvent.getAantalHerinnering();
			herinneringLogEvent.setMelding(melding);
		}
		return herinneringLogEvent;
	}

}
