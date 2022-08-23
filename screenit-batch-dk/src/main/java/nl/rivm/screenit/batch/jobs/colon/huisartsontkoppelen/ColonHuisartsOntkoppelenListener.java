package nl.rivm.screenit.batch.jobs.colon.huisartsontkoppelen;

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

import nl.rivm.screenit.batch.jobs.helpers.BaseLogListener;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;

import org.springframework.stereotype.Component;

@Component
public class ColonHuisartsOntkoppelenListener extends BaseLogListener
{

	public static final String AANTAL_RONDES_ONTKOPPELD = "colon.aantalrondesontkoppeld";

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.HUISARTS_ONTKOPPELEN_DK_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.HUISARTS_ONTKOPPELEN_DK_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		var logEvent = new LogEvent();
		String melding = "Aantal DK rondes ontkoppeld: " + getJobExecution().getExecutionContext().getLong(AANTAL_RONDES_ONTKOPPELD, 0L);
		addMelding(logEvent, melding);
		return logEvent;
	}

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return Bevolkingsonderzoek.COLON;
	}
}
