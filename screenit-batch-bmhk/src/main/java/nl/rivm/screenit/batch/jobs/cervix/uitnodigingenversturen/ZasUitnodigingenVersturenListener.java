package nl.rivm.screenit.batch.jobs.cervix.uitnodigingenversturen;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.batch.jobs.helpers.BaseLogListener;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.CervixUitnodigingVersturenLogEvent;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.rivm.screenit.model.verwerkingverslag.cervix.CervixZasVersturenRapportage;
import nl.rivm.screenit.model.verwerkingverslag.cervix.CervixZasVersturenRapportageProjectEntry;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.batch.core.JobExecution;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;

public class ZasUitnodigingenVersturenListener extends BaseLogListener
{

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return Bevolkingsonderzoek.CERVIX;
	}

	@Override
	protected void beforeStarting(JobExecution jobExecution)
	{
		ExecutionContext context = jobExecution.getExecutionContext();
		for (BriefType briefType : BriefType.values())
		{
			context.putLong(briefType.name(), 0);
		}
		context.put(ZasUitnodigingenVersturenConstants.PROJECTENCOUNTERS, new ArrayList<ProjectCounterHolder>());
	}

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_ZAS_UITNODIGING_VERSTUREN_JOB_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_ZAS_UITNODIGING_VERSTUREN_JOB_AFGEROND;
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		return new CervixUitnodigingVersturenLogEvent();
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		CervixUitnodigingVersturenLogEvent event = (CervixUitnodigingVersturenLogEvent) super.eindLogging(jobExecution);
		ExecutionContext context = jobExecution.getExecutionContext();

		long totaalVerstuurd = 0;

		for (BriefType brieftype : CervixMonsterType.ZAS.getBrieftypen())
		{
			Long aantal = context.getLong(brieftype.name(), 0);

			CervixZasVersturenRapportage entry = new CervixZasVersturenRapportage();
			entry.setAantal(aantal);
			entry.setBriefType(brieftype);
			hibernateService.saveOrUpdate(entry);

			totaalVerstuurd += aantal;
			event.getRapportage().add(entry);
		}

		List<ProjectCounterHolder> projectCounterHolders = (List<ProjectCounterHolder>) context.get(ZasUitnodigingenVersturenConstants.PROJECTENCOUNTERS);
		for (ProjectCounterHolder projectCounterHolder : projectCounterHolders)
		{
			ProjectGroep projectGroep = hibernateService.get(ProjectGroep.class, projectCounterHolder.getProjectGroepId());
			CervixZasVersturenRapportageProjectEntry projectGroepEntry = new CervixZasVersturenRapportageProjectEntry();
			projectGroepEntry.setProjectGroep(projectGroep);
			projectGroepEntry.setAantal(projectCounterHolder.getAantalVerstuurd());
			hibernateService.saveOrUpdate(projectGroepEntry);

			event.getProjectGroepen().add(projectGroepEntry);
		}

		if (Level.INFO == event.getLevel())
		{
			event.setLevel(Level.INFO);
			event.setMelding("Er zijn " + totaalVerstuurd + " uitnodiging(en) verstuurd.");

		}
		return event;
	}
}
