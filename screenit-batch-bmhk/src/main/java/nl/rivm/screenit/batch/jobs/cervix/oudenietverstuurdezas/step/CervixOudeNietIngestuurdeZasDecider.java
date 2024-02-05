package nl.rivm.screenit.batch.jobs.cervix.oudenietverstuurdezas.step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import nl.rivm.screenit.batch.jobs.cervix.oudenietverstuurdezas.CervixOudeNietIngestuurdeZasConstants;

import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.job.flow.FlowExecutionStatus;
import org.springframework.batch.core.job.flow.JobExecutionDecider;
import org.springframework.stereotype.Component;

@Component
public class CervixOudeNietIngestuurdeZasDecider implements JobExecutionDecider
{
	@Override
	public FlowExecutionStatus decide(JobExecution jobExecution, StepExecution stepExecution)
	{
		if (jobExecution.getExecutionContext().containsKey(CervixOudeNietIngestuurdeZasConstants.PROJECT_ID))
		{
			return FlowExecutionStatus.FAILED;
		}
		else
		{
			return FlowExecutionStatus.COMPLETED;
		}
	}
}
