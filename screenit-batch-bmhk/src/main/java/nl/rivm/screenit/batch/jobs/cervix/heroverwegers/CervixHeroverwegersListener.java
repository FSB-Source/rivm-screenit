package nl.rivm.screenit.batch.jobs.cervix.heroverwegers;

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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.cervix.CervixBaseLogListener;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;

import org.springframework.batch.core.JobExecution;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class CervixHeroverwegersListener extends CervixBaseLogListener
{

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_HEROVERWEGERS_BRIEVEN_GENEREREN_START;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_HEROVERWEGERS_BRIEVEN_GENEREREN_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		var event = super.eindLogging(jobExecution);
		var context = jobExecution.getExecutionContext();
		Long totaalVerstuurd = context.getLong(BriefType.CERVIX_HEROVERWEGERS.name(), 0);
		LOG.info("Er zijn {} heroverwegersbriev(en) klaargezet.", totaalVerstuurd);
		return event;
	}
}
