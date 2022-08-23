package nl.rivm.screenit.batch.jobs.cervix.verrichtingen;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.cervix.CervixBaseLogListener;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;

import org.springframework.batch.core.JobExecution;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class CervixBepalenVerrichtingenListener extends CervixBaseLogListener
{
	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_BEPALEN_VERRICHTINGEN_START;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_BEPALEN_VERRICHTINGEN_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		var context = jobExecution.getExecutionContext();
		long hpvUitstrijkjeAantal = context.getLong(CervixBepalenVerrichtingenConstants.VERRICHTINGEN_LAB_HPV_UITSTRIJKJE_AANTAL_KEY, 0L);
		long hpvZasAantal = context.getLong(CervixBepalenVerrichtingenConstants.VERRICHTINGEN_LAB_HPV_ZAS_AANTAL_KEY, 0L);
		long cytologieUitstrijkjeAantal = context.getLong(CervixBepalenVerrichtingenConstants.VERRICHTINGEN_LAB_CYTOLOGIE_UITSTRIJKJE_AANTAL_KEY, 0L);
		long cytologieZasAantal = context.getLong(CervixBepalenVerrichtingenConstants.VERRICHTINGEN_LAB_CYTOLOGIE_ZAS_AANTAL_KEY, 0L);
		long cytologieVervolgUitstrijkjeAantal = context.getLong(CervixBepalenVerrichtingenConstants.VERRICHTINGEN_LAB_CYTOLOGIE_VERVOLGUITSTRIJKJE_AANTAL_KEY, 0L);
		long huisartsUitstrijkjeAantal = context.getLong(CervixBepalenVerrichtingenConstants.VERRICHTINGEN_HUISARTS_UITSTRIJKJE_AANTAL_KEY, 0L);
		long totaal = hpvUitstrijkjeAantal + hpvZasAantal + cytologieUitstrijkjeAantal + cytologieZasAantal + cytologieVervolgUitstrijkjeAantal + huisartsUitstrijkjeAantal;
		LOG.info("Aantal hpv analyse uitstrijkje verrichtingen aangemaakt: {}", hpvUitstrijkjeAantal);
		LOG.info("Aantal hpv analyse zas verrichtingen aangemaakt: {}", hpvZasAantal);
		LOG.info("Aantal cytologie na uitstrijkje verrichtingen aangemaakt: {}", cytologieUitstrijkjeAantal);
		LOG.info("Aantal cytologie na zas verrichtingen aangemaakt: {}", cytologieZasAantal);
		LOG.info("Aantal cytologie vervolguitstrijkje verrichtingen aangemaakt: {}", cytologieVervolgUitstrijkjeAantal);
		LOG.info("Aantal huisarts uitstrijkje verrichtingen aangemaakt: {}", huisartsUitstrijkjeAantal);

		var logEvent = super.eindLogging(jobExecution);

		logEvent.setMelding("Aantal verrichtingen aangemaakt: " + totaal);
		return logEvent;
	}
}
