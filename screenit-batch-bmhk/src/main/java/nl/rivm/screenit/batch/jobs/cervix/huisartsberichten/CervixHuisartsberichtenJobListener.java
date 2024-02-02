package nl.rivm.screenit.batch.jobs.cervix.huisartsberichten;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseLogListener;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.CervixHuisartsberichtenBeeindigdLogEvent;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.verwerkingverslag.cervix.CervixHuisartsberichtenRapportage;
import nl.rivm.screenit.model.verwerkingverslag.cervix.CervixHuisartsberichtenRapportageEntry;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.batch.core.JobExecution;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class CervixHuisartsberichtenJobListener extends BaseLogListener
{

	private final ICurrentDateSupplier dateSupplier;

	private final HibernateService hibernateService;

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_HUISARTSBERICHTEN_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_HUISARTSBERICHTEN_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		return new CervixHuisartsberichtenBeeindigdLogEvent();
	}

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return Bevolkingsonderzoek.CERVIX;
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		var context = jobExecution.getExecutionContext();
		var rapportage = new CervixHuisartsberichtenRapportage();

		rapportage.setDatumVerwerking(dateSupplier.getDate());
		rapportage.setAantalHuisartsBepaald(context.getLong(CervixHuisartsberichtenConstants.HUISARTS_BEPAALD, 0));
		rapportage.setAantalHuisartsKonNietWordenBepaald(context.getLong(CervixHuisartsberichtenConstants.HUISARTS_KON_NIET_WORDEN_BEPAALD, 0));
		rapportage.setAantalVerstuud(context.getLong(CervixHuisartsberichtenConstants.VERSTUURD, 0));
		rapportage.setAantalVersturenMislukt(context.getLong(CervixHuisartsberichtenConstants.VERSTUREN_MISLUKT, 0));
		rapportage.setAantalHuisartsOnbekend(context.getLong(CervixHuisartsberichtenConstants.HUISARTS_ONBEKEND, 0));

		Map<HuisartsBerichtType, CervixHuisartsberichtenRapportageEntry> rapportagePerHuisartsBerichtType = new HashMap<>();
		aantallenContextVerwerken(CervixHuisartsberichtenConstants.BERICHT_TYPE_VERSTUURD, new AantalVerwerker<HuisartsBerichtType>()
		{
			@Override
			protected void verwerk(HuisartsBerichtType huisartsBerichtType, long aantal)
			{
				CervixHuisartsberichtenRapportageEntry huisartsberichtenRapportageEntry = new CervixHuisartsberichtenRapportageEntry(rapportage, huisartsBerichtType);
				huisartsberichtenRapportageEntry.setAantalVerstuurd(aantal);
				rapportagePerHuisartsBerichtType.put(huisartsBerichtType, huisartsberichtenRapportageEntry);
			}
		});

		aantallenContextVerwerken(CervixHuisartsberichtenConstants.BERICHT_TYPE_VERSTUREN_MISLUKT, new AantalVerwerker<HuisartsBerichtType>()
		{
			@Override
			protected void verwerk(HuisartsBerichtType huisartsBerichtType, long aantal)
			{
				CervixHuisartsberichtenRapportageEntry huisartsberichtenRapportageEntry = rapportagePerHuisartsBerichtType.get(huisartsBerichtType);
				if (huisartsberichtenRapportageEntry == null)
				{
					huisartsberichtenRapportageEntry = new CervixHuisartsberichtenRapportageEntry(rapportage, huisartsBerichtType);
					huisartsberichtenRapportageEntry.setAantalVerstuurd(0);
				}
				huisartsberichtenRapportageEntry.setAantalVersturenMislukt(aantal);
				rapportagePerHuisartsBerichtType.put(huisartsBerichtType, huisartsberichtenRapportageEntry);
			}
		});

		rapportage.setEntries(new ArrayList<>(rapportagePerHuisartsBerichtType.values()));

		var huisartsberichtenBeeindigdLogEvent = (CervixHuisartsberichtenBeeindigdLogEvent) super.eindLogging(jobExecution);
		huisartsberichtenBeeindigdLogEvent.setRapportage(rapportage);
		if (rapportage.getAantalVersturenMislukt() > 0)
		{
			huisartsberichtenBeeindigdLogEvent.setMelding("Één of meer berichten kon niet worden verzonden");
			huisartsberichtenBeeindigdLogEvent.setLevel(Level.ERROR);
		}

		hibernateService.saveOrUpdate(rapportage);
		return huisartsberichtenBeeindigdLogEvent;
	}
}
