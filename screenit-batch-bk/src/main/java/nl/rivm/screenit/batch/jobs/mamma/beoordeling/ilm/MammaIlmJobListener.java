package nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Date;
import java.util.Map;

import nl.rivm.screenit.batch.jobs.helpers.BaseLogListener;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.logging.MammaIlmLogEvent;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaIlmBeeldenStatusRapportage;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaIlmBeeldenStatusRapportageEntry;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.batch.core.JobExecution;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;

public class MammaIlmJobListener extends BaseLogListener
{
	public static final String KEY_BEELDEN_VERWIJDERD_AANTAL = "beeldenVerwijderdAantal";

	public static final String KEY_RONDES_VERWIJDERD_AANTAL = "rondesVerwijderdAantal";

	public static final String KEY_BEELDEN_STATUS_ENTRIES = "beeldenStatusEntries";

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private HibernateService hibernateService;

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.MAMMA_ILM_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.MAMMA_ILM_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		return new MammaIlmLogEvent();
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		MammaIlmLogEvent logEvent = (MammaIlmLogEvent) super.eindLogging(jobExecution);
		ExecutionContext executionContext = getJobExecution().getExecutionContext();
		addMelding(logEvent,
			"Rondes beelden verwijderd: " + (executionContext.containsKey(KEY_BEELDEN_VERWIJDERD_AANTAL) ? executionContext.getLong(KEY_BEELDEN_VERWIJDERD_AANTAL) : 0));

		addMelding(logEvent,
			"Rondes dossier verwijderd: " + (executionContext.containsKey(KEY_RONDES_VERWIJDERD_AANTAL) ? executionContext.getLong(KEY_RONDES_VERWIJDERD_AANTAL) : 0));

		Map<Long, Date> map = (Map<Long, Date>) jobExecution.getExecutionContext().get(KEY_BEELDEN_STATUS_ENTRIES);
		long beeldenStatusAantal = map == null ? 0 : map.size();
		addMelding(logEvent,
			"Beelden met status te verwijderen: " + beeldenStatusAantal);
		if (beeldenStatusAantal > 0)
		{
			logEvent.setLevel(Level.WARNING);
		}

		MammaIlmBeeldenStatusRapportage rapportage = new MammaIlmBeeldenStatusRapportage();
		rapportage.setAantalBeelden(beeldenStatusAantal);
		logEvent.setRapportage(rapportage);

		if (map != null)
		{
			for (Map.Entry<Long, Date> entry : map.entrySet())
			{
				MammaIlmBeeldenStatusRapportageEntry rapportageEntry = new MammaIlmBeeldenStatusRapportageEntry();
				rapportageEntry.setUitnodigingsNr(entry.getKey());
				rapportageEntry.setRapportage(rapportage);
				rapportageEntry.setStatusDatum(entry.getValue());
				rapportage.getEntries().add(rapportageEntry);
				hibernateService.saveOrUpdate(rapportageEntry);
			}
		}
		hibernateService.saveOrUpdate(rapportage);
		hibernateService.saveOrUpdate(logEvent);

		return logEvent;
	}

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return Bevolkingsonderzoek.MAMMA;
	}
}
