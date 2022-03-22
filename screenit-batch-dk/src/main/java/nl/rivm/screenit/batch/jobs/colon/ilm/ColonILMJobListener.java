package nl.rivm.screenit.batch.jobs.colon.ilm;

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
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.envers.RevisionKenmerk;
import nl.rivm.screenit.model.logging.LogEvent;

import org.springframework.batch.core.JobExecution;

public class ColonILMJobListener extends BaseLogListener
{
	private static final String REVISION_KENMERK_CONTEXT = "ILM";

	@Override
	protected void beforeStarting(JobExecution jobExecution)
	{
		super.beforeStarting(jobExecution);
		registerRevisionKenmerk(REVISION_KENMERK_CONTEXT, RevisionKenmerk.VERWIJDERD_DOOR_ILM);
	}

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.COLON_ILM_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.COLON_ILM_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		unregisterRevisionKenmerk(REVISION_KENMERK_CONTEXT);
		return new LogEvent();
	}

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return Bevolkingsonderzoek.COLON;
	}
}
