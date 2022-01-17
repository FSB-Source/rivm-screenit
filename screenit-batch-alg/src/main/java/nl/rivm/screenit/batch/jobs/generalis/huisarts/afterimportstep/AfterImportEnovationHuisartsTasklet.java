package nl.rivm.screenit.batch.jobs.generalis.huisarts.afterimportstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import java.util.List;

import nl.rivm.screenit.batch.jobs.generalis.huisarts.EnovationHuisartsJobListener;
import nl.rivm.screenit.model.enums.JobType;
import nl.rivm.screenit.service.EnovationHuisartsService;
import nl.rivm.screenit.service.JobService;
import nl.rivm.screenit.service.ZorgmailImportVoortgang;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.beans.factory.annotation.Autowired;

public class AfterImportEnovationHuisartsTasklet implements Tasklet
{
	private static final Logger LOG = LoggerFactory.getLogger(AfterImportEnovationHuisartsTasklet.class);

	private StepExecution stepExecution;

	@Autowired
	private JobService jobService;

	@Autowired
	private EnovationHuisartsService ennovationHuisartsService;

	@Override
	public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) throws Exception
	{
		this.stepExecution = chunkContext.getStepContext().getStepExecution();

		ZorgmailImportVoortgang voortgang = (ZorgmailImportVoortgang) stepExecution.getJobExecution().getExecutionContext().get(EnovationHuisartsJobListener.ZM_BESTAND_VOORTGANG);

		List<String> klantnummers = voortgang.getKlantnummers();
		voortgang.setGeinactiveerdeHuisartsenAfter(ennovationHuisartsService.valideerKlantnummers(klantnummers));
		klantnummers.clear();

		startJob(JobType.HUISARTS_ONTKOPPELEN_JOB_DK);
		return RepeatStatus.FINISHED;
	}

	private void startJob(JobType jobType)
	{
		String jmsMessageID = jobService.startJob(jobType, null);
		if (jmsMessageID == null)
		{
			LOG.error("Failed to start afterjob: {}.", jobType);
			stepExecution.setExitStatus(ExitStatus.FAILED);
			stepExecution.setStatus(BatchStatus.FAILED);
		}
	}

}
