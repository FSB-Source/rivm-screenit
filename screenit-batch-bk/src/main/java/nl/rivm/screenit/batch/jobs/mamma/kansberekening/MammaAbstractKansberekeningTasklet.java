package nl.rivm.screenit.batch.jobs.mamma.kansberekening;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;

import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.beans.factory.annotation.Autowired;

public abstract class MammaAbstractKansberekeningTasklet implements Tasklet
{
	@Autowired
	protected MammaBaseKansberekeningService baseKansberekeningService;

	protected ExecutionContext context;

	@Override
	public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext)
	{
		context = chunkContext.getStepContext().getStepExecution().getJobExecution().getExecutionContext();
		execute();
		return RepeatStatus.FINISHED;
	}

	protected abstract void execute();
}
