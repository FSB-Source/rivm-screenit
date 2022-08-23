package nl.rivm.screenit.batch.jobs.mamma.palga.csvimport.step;

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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.BatchConstants;
import nl.rivm.screenit.dto.mamma.MammaPalgaCsvImportDto;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.service.mamma.MammaPalgaService;

import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemWriter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class MammaPalgaCsvImportWriter implements ItemWriter<MammaPalgaCsvImportDto>
{
	private JobExecution jobExecution;

	private static final int HEADER_ROW = 1;

	@Autowired
	private MammaPalgaService palgaService;

	@Override
	public void write(List<? extends MammaPalgaCsvImportDto> items) throws Exception
	{
		for (MammaPalgaCsvImportDto dto : items)
		{
			if (dto.getRegelNummer() != HEADER_ROW && !dto.isFout())
			{
				String logMeldingPrefix = "#" + dto.getRegelNummer() + ": ";
				try
				{
					String errorMessage = palgaService.verwerkImportDto(dto);
					if (errorMessage != null)
					{
						logMelding(logMeldingPrefix + errorMessage, null);
					}
					else
					{
						LOG.info(logMeldingPrefix + "verwerkt");
					}
				}
				catch (IllegalArgumentException e)
				{
					logMelding(logMeldingPrefix + "semantisch", e);
					throw e;
				}
				catch (Exception e)
				{
					logMelding(logMeldingPrefix + "technisch", e);
					throw e;
				}

			}
		}
	}

	private void logMelding(String errorMessage, Exception e)
	{
		ExecutionContext executionContext = jobExecution.getExecutionContext();

		String melding = (getExecutionContext().containsKey(BatchConstants.MELDING) ? getExecutionContext().getString(BatchConstants.MELDING) + errorMessage
			: "Er zijn een aantal fouten gevonden tijdens het uitvoeren van de import:<br>" + errorMessage) + "<br>";
		executionContext.putString(BatchConstants.MELDING, melding);
		executionContext.put(BatchConstants.LEVEL, Level.WARNING);
		if (e instanceof IllegalArgumentException)
		{
			LOG.error(errorMessage + ". " + e.getMessage());
		}
		else if (e != null)
		{
			LOG.error(errorMessage, e);
		}
	}

	protected ExecutionContext getExecutionContext()
	{
		return jobExecution.getExecutionContext();
	}

	@BeforeStep
	public void saveStepExecution(StepExecution stepExecution)
	{
		this.jobExecution = stepExecution.getJobExecution();
	}

}
