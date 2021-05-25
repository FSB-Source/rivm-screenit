package nl.rivm.screenit.batch.jobs.mamma.uitnodigen;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.batch.jobs.helpers.BaseLogListener;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.Rivm;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.logging.MammaUitnodigenLogEvent;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaUitnodigenRapportage;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.batch.core.JobExecution;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;

public class MammaUitnodigenListener extends BaseLogListener
{
	private static final String RAPPORTAGE_ID_KEY = "rapportageId";

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Autowired
	private InstellingService instellingService;

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.MAMMA_UITNODIGEN_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.MAMMA_UITNODIGEN_AFGEROND;
	}

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return Bevolkingsonderzoek.MAMMA;
	}

	@Override
	protected LogEvent getStartLogEvent()
	{
		return new LogEvent();
	}

	@Override
	protected LogEvent getEindLogEvent()
	{
		return new MammaUitnodigenLogEvent();
	}

	@Override
	protected LogEvent eindLogging(JobExecution jobExecution)
	{
		MammaUitnodigenLogEvent logEvent = (MammaUitnodigenLogEvent) super.eindLogging(jobExecution);

		ExecutionContext executionContext = jobExecution.getExecutionContext();
		if (executionContext.containsKey(RAPPORTAGE_ID_KEY))
		{
			final Long rapportageId = executionContext.getLong(RAPPORTAGE_ID_KEY);
			MammaUitnodigenRapportage rapportage = hibernateService.get(MammaUitnodigenRapportage.class, rapportageId);

			logEvent.setRapportage(rapportage);
		}

		return logEvent;
	}

	@Override
	protected void saveEindLogGebeurtenis(LogEvent logEvent)
	{
		final List<Instelling> dashboardOrganisaties = new ArrayList<>(instellingService.getActieveInstellingen(Rivm.class));

		MammaUitnodigenRapportage rapportage = ((MammaUitnodigenLogEvent) logEvent).getRapportage();
		if (rapportage != null)
		{
			rapportage.getStandplaatsRondeUitnodigenRapportages().stream()
				.map(uitnodigenRapportageEntry -> uitnodigenRapportageEntry.getStandplaatsRonde().getStandplaats().getRegio())
				.distinct()
				.forEach(dashboardOrganisaties::add);
		}

		logService.logGebeurtenis(getEindLogGebeurtenis(), dashboardOrganisaties, logEvent, getBevolkingsonderzoek());
	}
}
