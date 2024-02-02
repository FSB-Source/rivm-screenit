package nl.rivm.screenit.batch.jobs.mamma.kansberekening;

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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseLogListener;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;

import org.springframework.batch.core.JobExecution;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class MammaKansberekeningListener extends BaseLogListener
{
	private final MammaBaseKansberekeningService kansberekeningService;

	@Override
	protected void beforeStarting(JobExecution jobExecution)
	{
		kansberekeningService.resetPreferences();
	}

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.MAMMA_KANSBEREKENING_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.MAMMA_KANSBEREKENING_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		ExecutionContext context = jobExecution.getExecutionContext();

		long standplaatsRondeDeelnameGemiddelden = context.getLong(MammaKansberekeningConstants.STANDPLAATS_RONDE_DEELNAME_GEMIDDELDEN_KEY, 0);
		long regioDeelnameGemiddelden = context.getLong(MammaKansberekeningConstants.REGIO_DEELNAME_GEMIDDELDEN_KEY, 0);
		long standplaatsRondeOpkomstGemiddelden = context.getLong(MammaKansberekeningConstants.STANDPLAATS_RONDE_OPKOMST_GEMIDDELDEN_KEY, 0);
		long regioOpkomstGemiddelden = context.getLong(MammaKansberekeningConstants.REGIO_OPKOMST_GEMIDDELDEN__KEY, 0);
		long screeningRondeSamples = context.getLong(MammaKansberekeningConstants.SCREENING_RONDE_SAMPLES_KEY, 0);
		long screeningRondeEvents = context.getLong(MammaKansberekeningConstants.SCREENING_RONDE_EVENTS_KEY, 0);
		long afspraakSamples = context.getLong(MammaKansberekeningConstants.AFSPRAAK_SAMPLES_KEY, 0);
		long afspraakEvents = context.getLong(MammaKansberekeningConstants.AFSPRAAK_EVENTS_KEY, 0);

		LogEvent logEvent = super.eindLogging(jobExecution);
		logEvent.setMelding("Bijgewerkt" +
			" screeningronde samples: " + screeningRondeSamples +
			", screeningronde events: " + screeningRondeEvents +
			", afspraak samples: " + afspraakSamples +
			", afspraak events: " + afspraakEvents +
			", standplaatsronde deelname gemiddelden: " + standplaatsRondeDeelnameGemiddelden +
			", regio deelname gemiddelden: " + regioDeelnameGemiddelden +
			", standplaatsronde opkomst gemiddelden: " + standplaatsRondeOpkomstGemiddelden +
			", regio opkomst gemiddelden: " + regioOpkomstGemiddelden);

		return logEvent;
	}

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return Bevolkingsonderzoek.MAMMA;
	}
}
