package nl.rivm.screenit.batch.jobs.colon.ifobtinlezen;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import nl.rivm.screenit.batch.BaseCsvFileReader;
import nl.rivm.screenit.batch.jobs.helpers.BaseLogListener;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.IfobtVerwerkingBeeindigdLogEvent;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.verwerkingverslag.IfobtVerwerkingRapportage;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;

import org.springframework.batch.core.JobExecution;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;

public class IfobtInlezenListener extends BaseLogListener
{

	@Autowired
	private LogService logService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	protected void beforeStarting(JobExecution jobExecution)
	{
		IfobtVerwerkingBeeindigdLogEvent verwerkingLogEvent = new IfobtVerwerkingBeeindigdLogEvent();
		IfobtVerwerkingRapportage rapportage = new IfobtVerwerkingRapportage();
		rapportage.setDatumVerwerking(currentDateSupplier.getDate());
		verwerkingLogEvent.setRapportage(rapportage);
		jobExecution.getExecutionContext().put(IfobtInlezenConstants.RAPPORTAGEKEYINLEZEN, verwerkingLogEvent);
	}

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.IFOBT_INLEZEN_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.IFOBT_INLEZEN_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		ExecutionContext context = getJobExecution().getExecutionContext();
		if (context.containsKey(IfobtInlezenConstants.RAPPORTAGEKEYINLEZEN))
		{
			return (IfobtVerwerkingBeeindigdLogEvent) context.get(IfobtInlezenConstants.RAPPORTAGEKEYINLEZEN);
		}
		return null;
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
		ExecutionContext executionContext = jobExecution.getExecutionContext();
		if (executionContext.containsKey(BaseCsvFileReader.RAPPORTAGEKEYFOUTINBESTAND))
		{
			logEvent.setLevel(Level.WARNING);
			addMelding(logEvent, executionContext.getString(BaseCsvFileReader.RAPPORTAGEKEYFOUTINBESTAND));
		}

		Boolean hasSentinel = (Boolean) executionContext.get(IfobtInlezenConstants.RAPPORTAGEKEYHASSENTINEL);
		if (Boolean.TRUE.equals(hasSentinel))
		{
			logService.logGebeurtenis(LogGebeurtenis.SENTINEL_ONTVANGEN, (Account) null, Bevolkingsonderzoek.COLON);
		}
		Boolean hasInterneTest = (Boolean) executionContext.get(IfobtInlezenConstants.RAPPORTAGEKEYHASINTERNETEST);
		if (Boolean.TRUE.equals(hasInterneTest))
		{
			logService.logGebeurtenis(LogGebeurtenis.INTERNE_TEST_ONTVANGEN, (Account) null, Bevolkingsonderzoek.COLON);
		}
		Boolean hasExerneTest = (Boolean) executionContext.get(IfobtInlezenConstants.RAPPORTAGEKEYHASEXTERNETEST);
		if (Boolean.TRUE.equals(hasExerneTest))
		{
			logService.logGebeurtenis(LogGebeurtenis.EXTERNE_TEST_ONTVANGEN, (Account) null, Bevolkingsonderzoek.COLON);
		}
		return logEvent;
	}

}
