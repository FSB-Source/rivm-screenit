package nl.rivm.screenit.batch.jobs.colon.uitnodigingenversturen;

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

import nl.rivm.screenit.batch.jobs.helpers.BaseLogListener;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.SelectieType;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.logging.UitnodigingVersturenLogEvent;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.rivm.screenit.model.verwerkingverslag.SelectieRapportage;
import nl.rivm.screenit.model.verwerkingverslag.SelectieRapportageEntry;
import nl.rivm.screenit.model.verwerkingverslag.SelectieRapportageProjectGroepEntry;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.batch.core.JobExecution;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;

public class UitnodigingenVersturenListener extends BaseLogListener
{

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	protected void beforeStarting(JobExecution jobExecution)
	{
		ExecutionContext context = jobExecution.getExecutionContext();
		for (ColonUitnodigingCategorie categorie : ColonUitnodigingCategorie.getCategorieen())
		{
			context.putLong(categorie.name(), 0);
		}
		context.put(UitnodigingenVersturenConstants.PROJECTENCOUNTERS, new ArrayList<UitnodigingenVersturenProjectGroepCounterHolder>());
	}

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return Bevolkingsonderzoek.COLON;
	}

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.UITNODIGING_VERSTUREN_JOB_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.UITNODIGING_VERSTUREN_JOB_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		return new UitnodigingVersturenLogEvent();
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		UitnodigingVersturenLogEvent event = (UitnodigingVersturenLogEvent) super.eindLogging(jobExecution);
		ExecutionContext context = jobExecution.getExecutionContext();

		long totaalVerstuurd = 0;

		SelectieRapportage rapportage = new SelectieRapportage();
		for (ColonUitnodigingCategorie categorie : ColonUitnodigingCategorie.getCategorieen())
		{
			Long aantal = context.getLong(categorie.name());

			SelectieRapportageEntry entry = new SelectieRapportageEntry();
			entry.setSelectieType(SelectieType.UITNODIGING_VERSTUURD);
			entry.setAantal(aantal);
			entry.setRapportage(rapportage);
			entry.setColonUitnodigingCategorie(categorie);
			hibernateService.saveOrUpdate(entry);
			totaalVerstuurd += aantal;
		}
		rapportage.setDatumVerwerking(currentDateSupplier.getDate());

		List<UitnodigingenVersturenProjectGroepCounterHolder> projectCounterHolders = (List<UitnodigingenVersturenProjectGroepCounterHolder>) context
			.get(UitnodigingenVersturenConstants.PROJECTENCOUNTERS);
		List<SelectieRapportageProjectGroepEntry> rapportageProjectGroepen = new ArrayList<SelectieRapportageProjectGroepEntry>();
		for (UitnodigingenVersturenProjectGroepCounterHolder projectCounterHolder : projectCounterHolders)
		{
			ProjectGroep projectGroep = hibernateService.get(ProjectGroep.class, projectCounterHolder.getProjectGroepId());
			SelectieRapportageProjectGroepEntry projectGroepEntry = new SelectieRapportageProjectGroepEntry();
			projectGroepEntry.setProjectGroep(projectGroep);
			projectGroepEntry.setAantal(projectCounterHolder.getAantalVerstuurd());
			projectGroepEntry.setRapportage(rapportage);
			hibernateService.saveOrUpdate(projectGroepEntry);
			rapportageProjectGroepen.add(projectGroepEntry);
		}
		rapportage.setProjectGroepen(rapportageProjectGroepen);

		hibernateService.saveOrUpdate(rapportage);
		event.setRapportage(rapportage);

		if (Level.INFO == event.getLevel())
		{
			if (totaalVerstuurd == 0)
			{
				event.setLevel(Level.WARNING);
				event.setMelding("Er stonden geen uitnodigingen klaar om naar het inpakcentrum te sturen.");
			}
			else
			{
				event.setLevel(Level.INFO);
				event.setMelding("Er zijn " + totaalVerstuurd + " uitnodiging(en) verstuurd.");
			}
		}
		return event;
	}
}
