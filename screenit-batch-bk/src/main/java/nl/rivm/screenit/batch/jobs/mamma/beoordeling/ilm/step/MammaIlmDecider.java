package nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import java.util.Date;

import lombok.Setter;

import nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.MammaIlmJobListener;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.job.flow.FlowExecutionStatus;
import org.springframework.batch.core.job.flow.JobExecutionDecider;

public class MammaIlmDecider implements JobExecutionDecider
{
	@Setter
	private String stepKey;

	@Setter
	private boolean observesTimeLimit;

	@Override
	public FlowExecutionStatus decide(JobExecution jobExecution, StepExecution stepExecution)
	{
		boolean executeStep = jobExecution.getExecutionContext().containsKey(stepKey) && (boolean) jobExecution.getExecutionContext().get(stepKey);
		if (executeStep && !hasExceededTimeLimit(jobExecution) && !hasProcessedAllScreeningRondes(jobExecution))
		{
			return FlowExecutionStatus.COMPLETED; 
		}
		else
		{
			return FlowExecutionStatus.FAILED; 
		}
	}

	private boolean hasExceededTimeLimit(JobExecution jobExecution)
	{
		if (observesTimeLimit && jobExecution.getExecutionContext().containsKey(MammaIlmJobListener.KEY_MAX_EIND_TIJD))
		{
			Date maxEndTime = (Date) jobExecution.getExecutionContext().get(MammaIlmJobListener.KEY_MAX_EIND_TIJD);
			Date nu = new Date();
			return DateUtil.compareAfter(nu, maxEndTime);
		}
		return false;
	}

	private boolean hasProcessedAllScreeningRondes(JobExecution jobExecution)
	{
		boolean hasProcessed = observesTimeLimit && jobExecution.getExecutionContext().containsKey(MammaIlmJobListener.KEY_RONDES_VERWERKT_AANTAL)
			&& jobExecution.getExecutionContext().getLong(MammaIlmJobListener.KEY_RONDES_VERWERKT_AANTAL) != MammaIlmJobListener.MAX_AANTAL_RONDES_VERWERKEN_IN_STEP;
		if (observesTimeLimit)
		{
			jobExecution.getExecutionContext().putLong(MammaIlmJobListener.KEY_RONDES_VERWERKT_AANTAL, 0);
		}
		return hasProcessed;
	}
}
