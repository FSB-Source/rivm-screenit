package nl.rivm.screenit.batch.jobs.colon.intake;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.job.flow.FlowExecutionStatus;
import org.springframework.batch.core.job.flow.JobExecutionDecider;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class IntakeAfsprakenMakenDecider implements JobExecutionDecider
{

	@Override
	public FlowExecutionStatus decide(JobExecution jobExecution, StepExecution stepExecution)
	{
		Integer ronde = (Integer) jobExecution.getExecutionContext().get(IntakeAfsprakenMakenConstants.HUIDIGE_RONDE);
		if (ronde == null)
		{
			ronde = 0;
		}
		else
		{
			++ronde;
		}

		Boolean allesVerwerkt = (Boolean) jobExecution.getExecutionContext().get(IntakeAfsprakenMakenConstants.ALLE_INTAKES_VERWERKT);

		if (!Boolean.TRUE.equals(allesVerwerkt))
		{
			LOG.info("Ronde gestart voor \"Intake afspraken maken job\". Ronde: " + ronde);
			jobExecution.getExecutionContext().putInt(IntakeAfsprakenMakenConstants.HUIDIGE_RONDE, ronde);
			return FlowExecutionStatus.FAILED;
		}
		else
		{
			return FlowExecutionStatus.COMPLETED;
		}
	}
}
