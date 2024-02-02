package nl.rivm.screenit.batch.jobs.colon.selectie.maxleeftijdpushuitnodigingstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.colon.selectie.SelectieConstants;

import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.job.flow.FlowExecutionStatus;
import org.springframework.batch.core.job.flow.JobExecutionDecider;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class UitnodigingPushMaxLeeftijdDecider implements JobExecutionDecider
{

	@Override
	public FlowExecutionStatus decide(JobExecution jobExecution, StepExecution stepExecution)
	{
		var executionContext = jobExecution.getExecutionContext();
		var ronde = (Integer) executionContext.get(SelectieConstants.PUSH_MAX_LEEFTIJD_COUNT);
		if (ronde == null)
		{
			ronde = 1;
		}
		else
		{
			++ronde;
		}

		var aantalDagenGeledenRondesGemaakt = (Integer) executionContext.get(SelectieConstants.LAATSTE_RONDE_AANTAL_DAGEN_GELEDEN_GEMAAKT);

		if (aantalDagenGeledenRondesGemaakt >= ronde)
		{
			LOG.info("Ronde: " + ronde);
			executionContext.putInt(SelectieConstants.PUSH_MAX_LEEFTIJD_COUNT, ronde);
			return FlowExecutionStatus.FAILED;
		}
		else
		{
			executionContext.putInt(SelectieConstants.PUSH_MAX_LEEFTIJD_COUNT, 0);
			return FlowExecutionStatus.COMPLETED;
		}
	}
}
