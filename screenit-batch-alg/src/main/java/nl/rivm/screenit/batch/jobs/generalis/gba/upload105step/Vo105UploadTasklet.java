package nl.rivm.screenit.batch.jobs.generalis.gba.upload105step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.File;

import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.generalis.gba.GbaConstants;
import nl.rivm.screenit.batch.jobs.generalis.gba.verwerk105step.Vo105ItemWriter;
import nl.rivm.screenit.model.gba.GbaVerwerkingsLog;

import org.apache.commons.io.FileUtils;
import org.jetbrains.annotations.NotNull;
import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.StepExecutionListener;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Component
public class Vo105UploadTasklet implements Tasklet, StepExecutionListener
{
	@Autowired
	private ApplicationContext applicationContext;

	@Setter
	private StepExecution stepExecution;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public RepeatStatus execute(@NotNull StepContribution contribution, ChunkContext chunkContext)
	{
		var executionContext = chunkContext.getStepContext().getJobExecutionContext();
		GbaVerwerkingsLog verwerkingsLog = (GbaVerwerkingsLog) executionContext.get(GbaConstants.RAPPORTAGEKEYGBA);
		String vo105Bestand = (String) executionContext.get(Vo105ItemWriter.VO105_BESTAND_KEY);

		if (vo105Bestand != null)
		{
			var vo105Uploader = applicationContext.getBean(Vo105Uploader.class);
			vo105Uploader.initialize(vo105Bestand, stepExecution.getJobExecution().getId(), verwerkingsLog);
			vo105Uploader.run();
			FileUtils.deleteQuietly(new File(vo105Bestand));
		}

		return RepeatStatus.FINISHED;
	}

	@Override
	public void beforeStep(@NotNull StepExecution stepExecution)
	{
		this.stepExecution = stepExecution;
	}

	@Override
	public ExitStatus afterStep(@NotNull StepExecution stepExecution)
	{
		return ExitStatus.COMPLETED;
	}
}
