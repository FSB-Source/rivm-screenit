package nl.rivm.screenit.batch.jobs.colon.selectie;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseLogListener;
import nl.rivm.screenit.batch.repository.ColonScreeningRondeRepository;
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
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.JobExecution;
import org.springframework.stereotype.Component;

import com.google.common.primitives.Ints;

@Component
@AllArgsConstructor
public class SelectieListener extends BaseLogListener
{

	private final LogService logService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final HibernateService hibernateService;

	private final ColonScreeningRondeRepository screeningRondeRepository;

	private final ColonDossierBaseService dossierService;

	@Override
	protected void beforeStarting(JobExecution jobExecution)
	{
		var selectieRapportage = new SelectieRapportage();
		selectieRapportage.setDatumVerwerking(currentDateSupplier.getDate());

		var selectieRapportageEntry = new SelectieRapportageEntry();
		selectieRapportageEntry.setAantal(0L);
		selectieRapportageEntry.setWaarvanGepusht(0L);
		selectieRapportageEntry.setRapportage(selectieRapportage);
		selectieRapportageEntry.setColonUitnodigingCategorie(ColonUitnodigingCategorie.U2_4);
		selectieRapportageEntry.setSelectieType(SelectieType.UITNODIGING_GEMAAKT);
		selectieRapportage.getEntries().add(selectieRapportageEntry);

		for (var colonUitnodigingCategorie : ColonUitnodigingCategorie.getCategorieen())
		{
			selectieRapportageEntry = new SelectieRapportageEntry();
			selectieRapportageEntry.setAantal(0L);
			selectieRapportageEntry.setWaarvanGepusht(0L);
			selectieRapportageEntry.setRapportage(selectieRapportage);
			selectieRapportageEntry.setColonUitnodigingCategorie(colonUitnodigingCategorie);
			selectieRapportageEntry.setSelectieType(SelectieType.UITNODIGING_GEMAAKT);
			selectieRapportage.getEntries().add(selectieRapportageEntry);
		}

		hibernateService.saveOrUpdate(selectieRapportage);
		dossierService.updateIntervalReferentieDatums();
		var executionContext = jobExecution.getExecutionContext();
		executionContext.putLong(SelectieConstants.RAPPORTAGEKEYSELECTIE, selectieRapportage.getId());
		executionContext.putInt(SelectieConstants.COLONSELECTIEWAARSCHUWINGIFOBTS, 0);
		executionContext.putInt(SelectieConstants.COLONSELECTIEMAXIMAALIFOBTS, 0);
		executionContext.putInt(SelectieConstants.LAATSTE_RONDE_AANTAL_DAGEN_GELEDEN_GEMAAKT, bepaalAantalDagenGeledenLaatsteRondeGemaakt());
	}

	private int bepaalAantalDagenGeledenLaatsteRondeGemaakt()
	{
		var ronde = screeningRondeRepository.findFirstByOrderByCreatieDatumDesc();
		long aantalDagenGeledenLaatsteRondeGemaakt = 0;
		if (ronde != null)
		{
			aantalDagenGeledenLaatsteRondeGemaakt = Math.max(1,
				Math.abs(ChronoUnit.DAYS.between(currentDateSupplier.getLocalDate(), DateUtil.toLocalDate(ronde.getCreatieDatum()))));
		}
		return Ints.checkedCast(aantalDagenGeledenLaatsteRondeGemaakt);
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
		var context = jobExecution.getExecutionContext();
		int waarschuwingClienten = context.getInt(SelectieConstants.COLONSELECTIEWAARSCHUWINGIFOBTS);
		int maximaalClienten = context.getInt(SelectieConstants.COLONSELECTIEMAXIMAALIFOBTS);
		if (waarschuwingClienten != 0 || maximaalClienten != 0)
		{
			LogEvent logEvent = getLogEventLimietIfobts(waarschuwingClienten, maximaalClienten);
			logService.logGebeurtenis(LogGebeurtenis.COLON_LIMIET_UITNODIGINGEN, logEvent, Bevolkingsonderzoek.COLON);
		}

		List<Long> gemeenteLongs = new ArrayList<Long>();
		for (var stepExecution : getJobExecution().getStepExecutions())
		{
			var stepContext = stepExecution.getExecutionContext();
			if (stepContext.containsKey(SelectieConstants.GEMEENTE_ZONDER_SCREENING_ORGANISATIES))
			{
				Map<Long, String> map = (Map<Long, String>) stepContext.get(SelectieConstants.GEMEENTE_ZONDER_SCREENING_ORGANISATIES);
				for (Entry<Long, String> entry : map.entrySet())
				{
					if (!gemeenteLongs.contains(entry.getKey()))
					{
						var logEvent = new LogEvent("Gemeente " + entry.getValue() + " is niet gekoppeld aan een screeningsorganisatie.");
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
		var logEvent = new LogEvent();
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
		var result = new SelectieRondeBeeindigdLogEvent();
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
		var logEvent = getEindLogEvent();
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
