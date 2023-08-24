package nl.rivm.screenit.batch.jobs.mamma.conceptmodel.step.resetconcept;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import javax.annotation.PostConstruct;

import nl.rivm.screenit.dto.mamma.planning.PlanningRestConstants;
import nl.rivm.screenit.util.rest.RestApiFactory;

import org.apache.commons.lang3.StringUtils;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

@Component
public class MammaConceptModelResettenTasklet implements Tasklet
{

	@Autowired
	@Qualifier("planningBkRestUrl")
	private String planningBkRestUrl;

	@PostConstruct
	public void init()
	{
		if (StringUtils.isNotBlank(planningBkRestUrl) && !planningBkRestUrl.endsWith("/"))
		{
			planningBkRestUrl += "/";
		}
	}

	@Override
	public RepeatStatus execute(StepContribution stepContribution, ChunkContext chunkContext)
	{
		RestTemplate restApi = RestApiFactory.create();
		restApi.postForEntity(planningBkRestUrl + PlanningRestConstants.C_ACTIE + "/" + PlanningRestConstants.C_READMODEL, null, String.class);

		return RepeatStatus.FINISHED;
	}
}
