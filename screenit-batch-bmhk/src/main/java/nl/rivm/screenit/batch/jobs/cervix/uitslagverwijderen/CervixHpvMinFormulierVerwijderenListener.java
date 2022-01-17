package nl.rivm.screenit.batch.jobs.cervix.uitslagverwijderen;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.item.ExecutionContext;

public class CervixHpvMinFormulierVerwijderenListener extends BaseLogListener
{

	private static final Logger LOG = LoggerFactory.getLogger(CervixHpvMinFormulierVerwijderenListener.class);

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_HPVMIN_UITSLAGEN_VERWIJDEREN_START;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_HPVMIN_UITSLAGEN_VERWIJDEREN_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return Bevolkingsonderzoek.CERVIX;
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		LogEvent event = super.eindLogging(jobExecution);

		ExecutionContext context = jobExecution.getExecutionContext();
		long formulierenGewist = context.getLong(CervixHpvMinFormulierVerwijderenConstants.AANTAL_FORMULIEREN_GEWIST, 0);
		if (StringUtils.isBlank(event.getMelding()))
		{
			String melding = "Er zijn " + formulierenGewist + " hpv(-) formulieren gewist.";
			event.setMelding(melding);
			LOG.info(melding);
		}
		return event;
	}
}
