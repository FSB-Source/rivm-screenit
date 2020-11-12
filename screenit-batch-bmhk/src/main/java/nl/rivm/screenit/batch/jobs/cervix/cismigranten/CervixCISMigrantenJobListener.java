package nl.rivm.screenit.batch.jobs.cervix.cismigranten;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.item.ExecutionContext;

public class CervixCISMigrantenJobListener extends BaseLogListener
{

	private static final Logger LOG = LoggerFactory.getLogger(CervixCISMigrantenJobListener.class);

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return Bevolkingsonderzoek.CERVIX;
	}

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_MIGRANTEN_UITNODIGEN_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_MIGRANTEN_UITNODIGEN_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		LogEvent selectieBeeindigdLogEvent = super.eindLogging(jobExecution);

		ExecutionContext context = jobExecution.getExecutionContext();
		long aantal = context.getLong(CervixCISMigrantenConstants.MIGRANT_UITNODIGINGEN_AANTAL_KEY, 0L);

		String melding = String.format("Er zijn %s client(en) uitgenodigd.", aantal);
		LOG.info(melding);
		selectieBeeindigdLogEvent.setMelding(melding);

		return selectieBeeindigdLogEvent;
	}
}
