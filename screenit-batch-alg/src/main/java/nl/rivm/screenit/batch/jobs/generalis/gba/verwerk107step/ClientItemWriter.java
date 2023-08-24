package nl.rivm.screenit.batch.jobs.generalis.gba.verwerk107step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import java.util.List;

import nl.rivm.screenit.batch.jobs.generalis.gba.GbaConstants;
import nl.rivm.screenit.batch.jobs.generalis.gba.exception.GbaImportException;
import nl.rivm.screenit.batch.service.GbaService;
import nl.rivm.screenit.model.gba.GbaVerwerkingsLog;
import nl.topicuszorg.gba.vertrouwdverbonden.model.Vo107Bericht;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.item.ItemWriter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ClientItemWriter implements ItemWriter<Vo107Bericht>
{
	@Autowired
	private GbaService gbaService;

	@Autowired
	private HibernateService hibernateService;

	private StepExecution stepExecution;

	@Override
	public void write(List<? extends Vo107Bericht> items)
	{
		GbaVerwerkingsLog verwerkingsLog = (GbaVerwerkingsLog) stepExecution.getJobExecution().getExecutionContext().get(GbaConstants.RAPPORTAGEKEYGBA);

		for (Vo107Bericht vo107Bericht : items)
		{
			try
			{
				gbaService.importVo107Bericht(vo107Bericht, verwerkingsLog);
			}
			catch (GbaImportException e)
			{
				gbaService.logGbaImportError(e, verwerkingsLog);
			}
		}

		hibernateService.getHibernateSession().flush();
		hibernateService.getHibernateSession().clear();
	}

	@BeforeStep
	public void saveStepExecution(StepExecution stepExecution)
	{
		this.stepExecution = stepExecution;
	}
}
