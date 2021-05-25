package nl.rivm.screenit.batch.jobs.mamma.beoordeling.discrepantie;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.batch.jobs.helpers.BaseLogListener;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.LogService;

import org.apache.commons.lang.StringUtils;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;

public class MammaDiscrepantieNaarArbitrageListener extends BaseLogListener
{
	public static final String MAMMA_RADIOLOGEN_BEOORDELINGEN_DOORGEZET_NAAR_ARBITRAGE = "mamma.beoordelingen.doorgezet";

	public static final String MAMMA_RADIOLOGEN_BEOORDELINGEN_DOORGEZET_NAAR_ARBITRAGE_AANTAL = "mamma.beoordelingen.doorgezet.aantal";

	@Autowired
	private LogService logService;

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.MAMMA_DISCREPANTIE_BEOORDELINGEN_DOORZETTEN_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.MAMMA_DISCREPANTIE_BEOORDELINGEN_DOORZETTEN_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		logService.logGebeurtenis(LogGebeurtenis.MAMMA_DISCREPANTIE_BEOORDELINGEN_DOORZETTEN, null, getResultLoggingString(jobExecution), Bevolkingsonderzoek.MAMMA);
		return super.eindLogging(jobExecution);
	}

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return Bevolkingsonderzoek.MAMMA;
	}

	private String getResultLoggingString(JobExecution jobExecution)
	{
		ExecutionContext context = jobExecution.getExecutionContext();
		Set<String> radiologen = (Set<String>) jobExecution.getExecutionContext().get(MAMMA_RADIOLOGEN_BEOORDELINGEN_DOORGEZET_NAAR_ARBITRAGE);
		long aantalBeoordelingen = context.getLong(MAMMA_RADIOLOGEN_BEOORDELINGEN_DOORGEZET_NAAR_ARBITRAGE_AANTAL, 0L);

		if (radiologen == null)
		{
			return "Er zijn geen beoordelingen met de status discrepantie die doorgezet moeten worden naar arbitrage.";
		}
		else
		{
			return "Er zijn in totaal " + aantalBeoordelingen + " discrepantie beoordelingen doorgezet naar arbitrage van de volgende radiologen: ["
				+ StringUtils.join(radiologen, ", ") + "]";
		}
	}
}
