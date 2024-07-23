package nl.rivm.screenit.batch.jobs.mamma.dense2.exporteerstestudieronde;

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

import java.io.IOException;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.mamma.dense2.MammaDense2Constants;
import nl.rivm.screenit.service.mamma.MammaBaseDense2Service;

import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class MammaDense2ExportEersteStudierondeTasklet implements Tasklet
{
	@Autowired
	private MammaBaseDense2Service dense2Service;

	@Override
	public RepeatStatus execute(@Nullable StepContribution contribution, ChunkContext chunkContext) throws IOException
	{
		var clienten = dense2Service.getExportClientenEersteStudieronde();
		chunkContext.getStepContext().getStepExecution().getJobExecution().getExecutionContext()
			.putLong(MammaDense2Constants.AANTAL_CLIENTEN_IN_EXPORT_EERSTE_STUDIERONDE, clienten.size());
		dense2Service.genereerCsv(clienten);

		return RepeatStatus.FINISHED;

	}
}
