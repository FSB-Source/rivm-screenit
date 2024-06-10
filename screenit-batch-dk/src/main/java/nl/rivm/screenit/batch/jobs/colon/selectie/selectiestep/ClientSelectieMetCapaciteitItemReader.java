package nl.rivm.screenit.batch.jobs.colon.selectie.selectiestep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import java.util.Collection;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.colon.selectie.SelectieConstants;
import nl.rivm.screenit.batch.service.ColonUitnodigingsgebiedCapaciteitService;
import nl.rivm.screenit.batch.service.impl.ColonUitnodigingsgebiedSelectieContext;
import nl.rivm.screenit.dao.colon.ColonUitnodigingsDao;
import nl.rivm.screenit.model.UitnodigingsGebied;
import nl.rivm.screenit.model.colon.ClientCategorieEntry;
import nl.rivm.screenit.model.enums.JobStartParameter;
import nl.rivm.screenit.model.verwerkingverslag.SelectieRapportage;
import nl.rivm.screenit.model.verwerkingverslag.SelectieRapportageGewijzigdGebiedEntry;
import nl.rivm.screenit.service.ProjectService;
import nl.rivm.screenit.service.colon.ColonUitnodigingService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.configuration.annotation.StepScope;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.SessionHolder;
import org.springframework.stereotype.Component;
import org.springframework.transaction.support.TransactionSynchronizationManager;

@Component
@StepScope
public class ClientSelectieMetCapaciteitItemReader extends AbstractClientSelectieReader
{

	@Autowired
	private ColonUitnodigingsDao uitnodigingsDao;

	@Autowired
	private ColonUitnodigingsgebiedCapaciteitService uitnodigingsGebiedCapactieitService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ProjectService projectService;

	@Autowired
	private ColonUitnodigingService uitnodigingService;

	private Collection<ColonUitnodigingsgebiedSelectieContext> uitnodigingsgebieden;

	public ClientSelectieMetCapaciteitItemReader()
	{
		super.setFetchSize(50);
	}

	@Override
	public ClientCategorieEntry read()
	{
		var clientCategorieEntry = getNextEntry();

		if (clientCategorieEntry == null)
		{
			rapporteerLeegLopendeGebieden();
		}
		return clientCategorieEntry;
	}

	@Override
	public void open(ExecutionContext executionContext) throws ItemStreamException
	{
		boolean unbindSessionFromThread = false;
		try
		{
			hibernateSession = sessionFactory.openSession();
			if (!TransactionSynchronizationManager.hasResource(sessionFactory))
			{
				TransactionSynchronizationManager.bindResource(sessionFactory, new SessionHolder(hibernateSession));
				unbindSessionFromThread = true;
			}
			JobParameters jobParameters = stepExecution.getJobExecution().getJobParameters();
			boolean herstartJob = Boolean.parseBoolean(jobParameters.getString(JobStartParameter.COLON_SELECTIE_HERSTART.name(), "false"));
			uitnodigingsgebieden = uitnodigingsGebiedCapactieitService.bepaalCapaciteit(executionContext, true, herstartJob);
			context = executionContext;
			setCursor();

		}
		finally
		{
			if (unbindSessionFromThread)
			{
				TransactionSynchronizationManager.unbindResource(sessionFactory);
			}
		}
	}

	private void setCursor()
	{
		var projectGroepen = projectService.getActieveProjectGroepenVoorUitnodigingDK();

		Integer minimaleLeeftijd = preferenceService.getInteger(PreferenceKey.MINIMALE_LEEFTIJD_COLON.name());
		if (minimaleLeeftijd == null)
		{
			throw new IllegalStateException("Minimale leeftijd colonscreening op de parameterisatie pagina is niet gezet.");
		}

		Integer maximaleLeeftijd = preferenceService.getInteger(PreferenceKey.MAXIMALE_LEEFTIJD_COLON.name());
		if (maximaleLeeftijd == null)
		{
			throw new IllegalStateException("Maximale leeftijd colonscreening op de parameterisatie pagina is niet gezet");
		}

		var selectieContext = new ColonClientSelectieContext();
		selectieContext.clientDao = clientDao;
		selectieContext.fitService = fitService;
		selectieContext.uitnodigingsDao = uitnodigingsDao;
		selectieContext.uitnodigingService = uitnodigingService;
		selectieContext.hibernateService = hibernateService;
		selectieContext.uitnodigingsGebiedCapaciteitService = uitnodigingsGebiedCapactieitService;
		selectieContext.fetchSize = fetchSize;
		selectieContext.minimaleLeeftijd = minimaleLeeftijd;
		selectieContext.maximaleLeeftijd = maximaleLeeftijd;
		selectieContext.init(uitnodigingService.getUitnodigingCohorten(), projectGroepen);

		cursor = new ClientSelectieMetCapaciteitItemCursor(selectieContext, uitnodigingsgebieden);
	}

	private void rapporteerLeegLopendeGebieden()
	{
		Long selectieRapportageId = stepExecution.getJobExecution().getExecutionContext().getLong(SelectieConstants.RAPPORTAGEKEYSELECTIE);
		var selectieRapportage = hibernateService.get(SelectieRapportage.class, selectieRapportageId);
		if (selectieRapportage != null)
		{
			for (var uitnodigingsgebied : uitnodigingsgebieden)
			{
				int capaciteitToevoegingOfOver = uitnodigingsgebied.getUitnodigingscapaciteitToevoegingOfOver();
				if (capaciteitToevoegingOfOver > 0 || uitnodigingsgebied.isLeeglopendGebied())
				{
					var gewijzigdGebiedEntry = new SelectieRapportageGewijzigdGebiedEntry();
					gewijzigdGebiedEntry.setPercentage(capaciteitToevoegingOfOver);
					gewijzigdGebiedEntry.setUitnodigingsGebied(hibernateService.load(UitnodigingsGebied.class, uitnodigingsgebied.getUitnodigingsgebiedId()));
					gewijzigdGebiedEntry.setRapportage(selectieRapportage);
					selectieRapportage.getGewijzigdeGebieden().add(gewijzigdGebiedEntry);
					hibernateService.saveOrUpdate(selectieRapportage);
				}
			}
		}
	}

}
