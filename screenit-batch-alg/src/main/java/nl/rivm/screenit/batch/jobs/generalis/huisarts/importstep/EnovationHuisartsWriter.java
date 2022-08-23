package nl.rivm.screenit.batch.jobs.generalis.huisarts.importstep;

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

import java.text.ParseException;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.BatchConstants;
import nl.rivm.screenit.batch.jobs.generalis.huisarts.EnovationHuisartsJobListener;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.service.ZorgmailImportMapping;
import nl.rivm.screenit.service.ZorgmailImportService;
import nl.rivm.screenit.service.ZorgmailImportVoortgang;

import org.apache.commons.lang3.StringUtils;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemWriter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class EnovationHuisartsWriter implements ItemWriter<Object[]>
{

	@Autowired
	private ZorgmailImportService zorgmailImportService;

	private ExecutionContext executionContext;

	@Override
	public void write(List<? extends Object[]> chunk) throws Exception
	{
		ZorgmailImportMapping mapping = (ZorgmailImportMapping) executionContext.get(EnovationHuisartsJobListener.ZM_BESTAND_MAPPING);
		ZorgmailImportVoortgang voortgang = (ZorgmailImportVoortgang) executionContext.get(EnovationHuisartsJobListener.ZM_BESTAND_VOORTGANG);
		for (Object[] lineObjects : chunk)
		{
			Integer lineNumber = (Integer) lineObjects[0];
			if (lineNumber > 1)
			{
				try
				{
					if (lineNumber % 100 == 0)
					{
						LOG.info("Verwerk line #" + lineNumber);
					}
					zorgmailImportService.verwerkLine((String[]) lineObjects[1], lineNumber, mapping, voortgang, true);
				}
				catch (IllegalStateException | ParseException | IndexOutOfBoundsException e)
				{
					String melding = "Regel #" + lineNumber + " is niet verwerkt";
					LOG.warn(melding, e);
					addMelding(melding);
				}
			}
		}
	}

	private void addMelding(String melding)
	{
		String huidigeMelding = "";
		if (executionContext.containsKey(BatchConstants.MELDING))
		{
			huidigeMelding = executionContext.getString(BatchConstants.MELDING);
		}

		if (melding == null)
		{
			melding = "Er is een onbekende fout opgetreden bij de verwerking van een set van zorgmailadressen. Neem contact op met de helpdesk.";
		}
		if (StringUtils.isBlank(huidigeMelding))
		{
			huidigeMelding = melding;
		}
		else if (!huidigeMelding.contains(melding))
		{
			huidigeMelding += "<br>" + melding;
		}
		executionContext.putString(BatchConstants.MELDING, huidigeMelding);
		executionContext.put(BatchConstants.LEVEL, Level.ERROR);
	}

	@BeforeStep
	public void saveStepExecution(StepExecution stepExecution)
	{
		this.executionContext = stepExecution.getJobExecution().getExecutionContext();
	}

}
