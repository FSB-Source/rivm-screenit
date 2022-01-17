package nl.rivm.screenit.batch.jobs.colon.selectie;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import nl.rivm.screenit.batch.jobs.helpers.BaseLogListener;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.SelectieType;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.logging.SelectieRondeBeeindigdLogEvent;
import nl.rivm.screenit.model.verwerkingverslag.SelectieRapportage;
import nl.rivm.screenit.model.verwerkingverslag.SelectieRapportageEntry;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;

public class SelectieListener extends BaseLogListener
{

	@Autowired
	private LogService logService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private HibernateService hibernateService;

	@Override
	protected void beforeStarting(JobExecution jobExecution)
	{
		SelectieRapportage selectieRapportage = new SelectieRapportage();
		selectieRapportage.setDatumVerwerking(currentDateSupplier.getDate());

		SelectieRapportageEntry entry = new SelectieRapportageEntry();
		entry.setAantal(0L);
		entry.setWaarvanGepusht(0L);
		entry.setRapportage(selectieRapportage);
		entry.setColonUitnodigingCategorie(ColonUitnodigingCategorie.U2_4);
		entry.setSelectieType(SelectieType.UITNODIGING_GEMAAKT);
		selectieRapportage.getEntries().add(entry);

		for (ColonUitnodigingCategorie colonUitnodigingCategorie : ColonUitnodigingCategorie.getCategorieen())
		{
			entry = new SelectieRapportageEntry();
			entry.setAantal(0L);
			entry.setWaarvanGepusht(0L);
			entry.setRapportage(selectieRapportage);
			entry.setColonUitnodigingCategorie(colonUitnodigingCategorie);
			entry.setSelectieType(SelectieType.UITNODIGING_GEMAAKT);
			selectieRapportage.getEntries().add(entry);
		}

		hibernateService.saveOrUpdate(selectieRapportage);
		jobExecution.getExecutionContext().putLong(SelectieConstants.RAPPORTAGEKEYSELECTIE, selectieRapportage.getId());
		jobExecution.getExecutionContext().putInt(SelectieConstants.COLONSELECTIEWAARSCHUWINGIFOBTS, 0);
		jobExecution.getExecutionContext().putInt(SelectieConstants.COLONSELECTIEMAXIMAALIFOBTS, 0);
	}

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.SELECTIERONDE_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.SELECTIERONDE_BEEINDIGD;
	}

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return Bevolkingsonderzoek.COLON;
	}

	@Override
	protected void beforeEindeLogging(JobExecution jobExecution)
	{
		ExecutionContext context = jobExecution.getExecutionContext();
		int waarschuwingClienten = context.getInt(SelectieConstants.COLONSELECTIEWAARSCHUWINGIFOBTS);
		int maximaalClienten = context.getInt(SelectieConstants.COLONSELECTIEMAXIMAALIFOBTS);
		if (waarschuwingClienten != 0 || maximaalClienten != 0)
		{
			LogEvent logEvent = getLogEventLimietIfobts(waarschuwingClienten, maximaalClienten);
			logService.logGebeurtenis(LogGebeurtenis.COLON_LIMIET_UITNODIGINGEN, logEvent, Bevolkingsonderzoek.COLON);
		}

		List<Long> gemeenteLongs = new ArrayList<Long>();
		for (StepExecution stepExecution : getJobExecution().getStepExecutions())
		{
			ExecutionContext stepContext = stepExecution.getExecutionContext();
			if (stepContext.containsKey(SelectieConstants.GEMEENTE_ZONDER_SCREENING_ORGANISATIES))
			{
				Map<Long, String> map = (Map<Long, String>) stepContext.get(SelectieConstants.GEMEENTE_ZONDER_SCREENING_ORGANISATIES);
				for (Entry<Long, String> entry : map.entrySet())
				{
					if (!gemeenteLongs.contains(entry.getKey()))
					{
						LogEvent logEvent = new LogEvent("Gemeente " + entry.getValue() + " is niet gekoppeld aan een screeningsorganisatie.");
						logEvent.setLevel(Level.WARNING);
						logService.logGebeurtenis(LogGebeurtenis.COLON_GEMEENTE_NIET_GEKOPPELD_AAN_SCREENING_ORGANISATIE, logEvent, Bevolkingsonderzoek.COLON);
						gemeenteLongs.add(entry.getKey());
					}
				}
			}
		}
	}

	private LogEvent getLogEventLimietIfobts(int waarschuwingClienten, int maximaalClienten)
	{
		LogEvent logEvent = new LogEvent();
		Level level = Level.INFO;
		String melding = "";
		if (waarschuwingClienten > 0)
		{
			melding += "Aantal cliënten geselecteerd waarbij de waarschuwingslimiet is overschreden: " + waarschuwingClienten + "; ";
			level = Level.WARNING;
		}
		if (maximaalClienten > 0)
		{
			melding += "Aantal cliënten geselecteerd waarbij de maximale waarde is overschreden: " + maximaalClienten + "; ";
			level = Level.ERROR;
		}
		logEvent.setLevel(level);
		logEvent.setMelding(melding);
		return logEvent;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		SelectieRondeBeeindigdLogEvent result = new SelectieRondeBeeindigdLogEvent();
		result.setResultaat(jobHasExitCode(ExitStatus.COMPLETED));
		result.setLevel(Level.INFO);
		if (getJobExecution().getExecutionContext().containsKey(SelectieConstants.RAPPORTAGEKEYSELECTIE))
		{
			result.setRapportage(hibernateService.load(SelectieRapportage.class, getJobExecution().getExecutionContext().getLong(SelectieConstants.RAPPORTAGEKEYSELECTIE)));
		}
		return result;
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		LogEvent logEvent = getEindLogEvent();
		if (logEvent != null)
		{
			SelectieRondeBeeindigdLogEvent result = (SelectieRondeBeeindigdLogEvent) logEvent;

			if (!jobExecution.getAllFailureExceptions().isEmpty())
			{
				for (Throwable throwable : jobExecution.getAllFailureExceptions())
				{
					result.getExceptionStackTrace().add(getStackTrace(throwable));
				}
				Throwable exception = jobExecution.getAllFailureExceptions().get(0);
				result.setMelding("De job heeft onsuccesvol gedraaid, neem contact op met de helpdesk.");
				if (exception instanceof IllegalStateException)
				{
					result.setMelding(exception.getMessage());
				}
				result.setLevel(Level.ERROR);
			}

			if (!Level.ERROR.equals(result.getLevel()))
			{
				SelectieRapportage rapportage = result.getRapportage();
				Long totaalAantalUitnodigingen = (long) 0;
				for (SelectieRapportageEntry entry : rapportage.getEntries())
				{
					if (SelectieType.UITNODIGING_GEMAAKT.equals(entry.getSelectieType()))
					{
						totaalAantalUitnodigingen += entry.getAantal();
					}
				}
				if (totaalAantalUitnodigingen == 0)
				{
					result.setLevel(Level.WARNING);
					result.setMelding("Er zijn geen uitnodigingen aangemaakt.");
				}
				else
				{
					result.setLevel(Level.INFO);
					result.setMelding("Er zijn " + totaalAantalUitnodigingen + " uitnodiging(en) aangemaakt.");
				}
			}
			hibernateService.saveOrUpdate(result);
			logEvent = result;
		}
		else
		{
			logEvent = new LogEvent();
			logEvent.setLevel(Level.ERROR);
			logEvent.setMelding("Logevent niet gevonden in de executionContext terwijl deze wel wordt verwacht");
		}
		return logEvent;
	}
}
