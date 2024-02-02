package nl.rivm.screenit.batch.jobs.mamma.beoordeling.status;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import java.util.Set;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseLogListener;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.LogService;

import org.apache.commons.lang.StringUtils;
import org.springframework.batch.core.JobExecution;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class MammaBeoordelingenAccorderenListener extends BaseLogListener
{
	public static final String MAMMA_RADIOLOGEN_BEOORDELINGEN_GEACCORDEERD = "mamma.beoordelingen.geaccordeerd.radiologen";

	public static final String MAMMA_RADIOLOGEN_BEOORDELINGEN_GEACCORDEERD_AANTAL = "mamma.beoordelingen.geaccordeerd.radiologen.aantal";

	private final LogService logService;

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.MAMMA_BEOORDELINGEN_GEACCORDEERD_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.MAMMA_BEOORDELINGEN_GEACCORDEERD_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		logService.logGebeurtenis(LogGebeurtenis.MAMMA_BEOORDELINGEN_GEACCORDEERD, null, getResultLoggingString(jobExecution), Bevolkingsonderzoek.MAMMA);

		return super.eindLogging(jobExecution);
	}

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return Bevolkingsonderzoek.MAMMA;
	}

	private String getResultLoggingString(JobExecution jobExecution)
	{
		var context = jobExecution.getExecutionContext();
		var radiologen = (Set<String>) context.get(MAMMA_RADIOLOGEN_BEOORDELINGEN_GEACCORDEERD);
		var aantalBeoordelingen = context.getLong(MAMMA_RADIOLOGEN_BEOORDELINGEN_GEACCORDEERD_AANTAL, 0L);

		if (radiologen == null)
		{
			return "Er zijn geen beoordelingen geaccordeerd.";
		}
		else
		{
			return "Er zijn in totaal " + aantalBeoordelingen + " beoordelingen geaccordeerd van de volgende radiologen: [" + StringUtils.join(radiologen, ", ") + "]";
		}
	}

}
