
package nl.rivm.screenit.batch.jobs.generalis.huisarts;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.batch.jobs.helpers.BaseLogListener;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.ZorgmailImportVoortgang;

import org.springframework.batch.core.JobExecution;
import org.springframework.stereotype.Component;

@Component
public class EnovationHuisartsJobListener extends BaseLogListener
{

	public static final String ZM_BESTAND_MAPPING = "key.zm.bestand.mapping";

	public static final String ZM_BESTAND_VOORTGANG = "key.zm.bestand.voortgang";

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.ENOVATION_HUISARTS_BATCH_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.ENOVATION_HUISARTS_BATCH_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		LogEvent logEvent = new LogEvent();
		ZorgmailImportVoortgang voortgang = (ZorgmailImportVoortgang) getJobExecution().getExecutionContext().get(ZM_BESTAND_VOORTGANG);
		String melding = "Importeren van zorgmail adresboek is afgerond. ";
		if (voortgang != null)
		{
			melding += "Totaal #records: " + voortgang.getTotaalAantalRijen() + ". Huisartsen nieuw: " + voortgang.getNieuweHuisartsen() + ", bijgewerkt: "
				+ voortgang.getGeupdateHuisartsen() + ", geïnactiveerd(door enovation): " + voortgang.getGeinactiveerdeHuisartsen();

			if (voortgang.getGeinactiveerdeHuisartsenAfter() > 0)
			{
				melding += ", geïnactiveerd(niet (meer) in bestand): " + voortgang.getGeinactiveerdeHuisartsenAfter();
			}
			melding += ".";
		}
		else
		{
			melding += " Geen bestand verwerkt.";
		}
		addMelding(logEvent, melding);
		return logEvent;
	}

	@Override
	protected Level getLevel(JobExecution execution)
	{
		Level level = super.getLevel(execution);
		if (level == Level.INFO)
		{
			ZorgmailImportVoortgang voortgang = (ZorgmailImportVoortgang) execution.getExecutionContext().get(ZM_BESTAND_VOORTGANG);
			if (voortgang.getGeinactiveerdeHuisartsenAfter() > 0)
			{
				level = Level.WARNING;
			}
		}
		return level;
	}

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return null;
	}

}
