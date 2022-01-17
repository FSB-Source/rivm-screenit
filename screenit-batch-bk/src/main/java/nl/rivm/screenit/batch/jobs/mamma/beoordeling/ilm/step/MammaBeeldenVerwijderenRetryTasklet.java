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

import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.MammaIlmJobListener;
import nl.rivm.screenit.batch.model.dto.MammaIlmRetryDto;
import nl.rivm.screenit.batch.service.MammaIlmService;
import nl.rivm.screenit.model.mamma.enums.MammaHL7BerichtType;
import nl.rivm.screenit.model.mamma.enums.MammaHL7v24ORMBerichtStatus;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaIlmBeeldenStatusRapportage;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaIlmBeeldenStatusRapportageEntry;
import nl.rivm.screenit.service.BerichtToBatchService;

import org.apache.commons.collections.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.beans.factory.annotation.Autowired;

public class MammaBeeldenVerwijderenRetryTasklet implements Tasklet
{
	@Autowired
	private MammaIlmService ilmService;

	@Autowired
	private BerichtToBatchService berichtToBatchService;

	private final Logger LOG = LoggerFactory.getLogger(MammaBeeldenVerwijderenRetryTasklet.class);

	@Override
	public RepeatStatus execute(StepContribution stepContribution, ChunkContext chunkContext) throws Exception
	{
		retryBeelden(stepContribution);

		return RepeatStatus.FINISHED;
	}

	private void retryBeelden(StepContribution stepContribution)
	{
		bepaalFailedRetries(stepContribution);
		List<MammaIlmRetryDto> dtoList = getDtoList(stepContribution);
		if (!CollectionUtils.isEmpty(dtoList))
		{
			dtoList.stream().filter(dto -> !dto.isFailedRetry()).forEach(retryEntry ->
			{

				LOG.info("Retry, statusDatum: {} uitnodigingsNr: {}, isBezwaar: {}, isUploaded: {}",
					retryEntry.getStatusDatum(), retryEntry.getAccessionNumber(), retryEntry.isBezwaar(), retryEntry.isUploaded());
				if (retryEntry.isUploaded())
				{
					berichtToBatchService.queueMammaUploadBeeldenHL7v24BerichtUitgaand(retryEntry.getAccessionNumber(), retryEntry.getClientId(),
						MammaHL7v24ORMBerichtStatus.DELETE,
						MammaHL7BerichtType.IMS_ORM_ILM_UPLOAD_BEELDEN);
				}
				else
				{
					berichtToBatchService.queueMammaIlmHl7v24BerichtUitgaand(retryEntry.getAccessionNumber(), retryEntry.getClientId(), MammaHL7v24ORMBerichtStatus.DELETE,
						MammaHL7BerichtType.IMS_ORM_ILM);
				}
			});
		}
	}

	private List<MammaIlmRetryDto> getDtoList(StepContribution stepContribution)
	{
		List<MammaIlmRetryDto> dtoList = (List<MammaIlmRetryDto>) stepContribution.getStepExecution().getJobExecution().getExecutionContext()
			.get(MammaIlmJobListener.KEY_BEELDEN_STATUS_ENTRIES);
		return dtoList;
	}

	private void bepaalFailedRetries(StepContribution stepContribution)
	{
		List<MammaIlmRetryDto> dtoList = getDtoList(stepContribution);

		if (dtoList != null)
		{
			MammaIlmBeeldenStatusRapportage meestRecenteStatusRapportage = ilmService.getMeestRecenteStatusRapportage();

			if (meestRecenteStatusRapportage != null)
			{
				List<Long> retriedAccessionNumbers = meestRecenteStatusRapportage.getEntries().stream()
					.map(MammaIlmBeeldenStatusRapportageEntry::getAccessionNumber)
					.collect(Collectors.toList());

				dtoList.forEach(entry -> entry.setFailedRetry(retriedAccessionNumbers.contains(entry.getAccessionNumber())));
			}
		}
	}

}
