package nl.rivm.screenit.batch.jobs.mamma.dense2;

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

import nl.rivm.screenit.batch.jobs.helpers.BaseLogListener;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;

import org.springframework.stereotype.Component;

@Component
public class MammaDense2Listener extends BaseLogListener
{

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.MAMMA_DENSE2_CSV_EXPORT_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.MAMMA_DENSE2_CSV_EXPORT_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		var aantalClientenT1 = getJobExecution().getExecutionContext().getLong(MammaDense2Constants.AANTAL_CLIENTEN_IN_EXPORT_EERSTE_STUDIERONDE, 0L);
		var aantalClientenT7 = getJobExecution().getExecutionContext().getLong(MammaDense2Constants.AANTAL_CLIENTEN_IN_EXPORT_TWEEDE_STUDIERONDE, 0L);
		var aantalOnderzoekenDensiteitVerwijderd = getJobExecution().getExecutionContext().getLong(MammaDense2Constants.AANTAL_ONDERZOEKEN_DENSITEIT_VERWIJDERD, 0L);
		var melding = "DENSE2 export afgehandeld: Eerste studieronde " + aantalClientenT1 + " populatie," + " tweede studieronde " + aantalClientenT7 + " populatie";
		if (aantalOnderzoekenDensiteitVerwijderd > 0)
		{
			melding += ", " + aantalOnderzoekenDensiteitVerwijderd + " onderzoek(en) waarvan densiteit is verwijderd";
		}
		return new LogEvent(melding);
	}

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return Bevolkingsonderzoek.MAMMA;
	}
}
