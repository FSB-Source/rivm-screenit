
package nl.rivm.screenit.batch.jobs.colon.intake.afsprakenmakenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.datasource.ReadOnlyDBActionsWithFallback;
import nl.rivm.screenit.batch.datasource.ReadOnlyDBActionsWithFallback.DelegatedReadOnlyDBActions;
import nl.rivm.screenit.batch.jobs.colon.intake.IntakeAfsprakenMakenConstants;
import nl.rivm.screenit.batch.model.ClientAfspraak;
import nl.rivm.screenit.batch.service.IntakeAfpraakService;
import nl.rivm.screenit.batch.service.PlanIntakeAfsprakenService;
import nl.rivm.screenit.model.colon.planning.VrijSlot;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.logging.IntakeMakenLogEvent;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.joda.time.DateTime;
import org.omg.CORBA.IntHolder;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemReader;
import org.springframework.batch.item.ItemStream;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.orm.hibernate5.SessionFactoryUtils;
import org.springframework.orm.hibernate5.SessionHolder;
import org.springframework.transaction.support.TransactionSynchronizationManager;

public class IntakeAfsprakenMakenReader implements ItemReader<ClientAfspraak>, ItemStream
{

	@Autowired
	private IntakeAfpraakService intakeAfspraakService;

	@Autowired
	private PlanIntakeAfsprakenService planIntakeAfsprakenService;

	private Iterator<ClientAfspraak> clienten;

	@Autowired
	private SessionFactory sessionFactory;

	private Session hibernateSession;

	private StepExecution stepExecution;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	@Qualifier("maximumRetrySecondsSpend")
	private Long maximumRetrySecondsSpend = Long.valueOf(10);

	@Autowired
	@Qualifier("afstandFactorRetry")
	private Integer afstandFactorRetry;

	@Override
	public ClientAfspraak read()
	{
		if (clienten.hasNext())
		{
			return clienten.next();
		}
		return null;
	}

	@Override
	public void open(ExecutionContext executionContext)
	{
		ReadOnlyDBActionsWithFallback.runReadOnlyDBActions(new DelegatedReadOnlyDBActions()
		{

			@Override
			public void doActions()
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

					ExecutionContext innerExecutionContext = stepExecution.getJobExecution().getExecutionContext();
					IntakeMakenLogEvent intakeMelding = (IntakeMakenLogEvent) innerExecutionContext.get(IntakeAfsprakenMakenConstants.RAPPORTAGEKEYINTAKE);
					int ronde = innerExecutionContext.getInt(IntakeAfsprakenMakenConstants.HUIDIGE_RONDE, 0);
					Integer maxRonde = preferenceService.getInteger(PreferenceKey.COLON_MAX_EXTRA_POGINGEN_PLANNING_INTAKE.name());
					List<ClientAfspraak> clientAfspraken;
					StringBuilder foutmeldingTextUitJobContext = new StringBuilder();
					if (innerExecutionContext.containsKey(IntakeAfsprakenMakenConstants.FOUT_BIJ_INTAKE_VASTLEGGEN))
					{
						foutmeldingTextUitJobContext.append(innerExecutionContext.get(IntakeAfsprakenMakenConstants.FOUT_BIJ_INTAKE_VASTLEGGEN));
					}
					if (ronde != 0 && ronde <= maxRonde)
					{
						clientAfspraken = intakeAfspraakService.getClientenVoorIntakeAfspraakMaken(afstandFactorRetry, 100 - afstandFactorRetry, foutmeldingTextUitJobContext);
					}
					else
					{
						clientAfspraken = intakeAfspraakService.getClientenVoorIntakeAfspraakMaken(foutmeldingTextUitJobContext);
					}
					innerExecutionContext.put(IntakeAfsprakenMakenConstants.FOUT_BIJ_INTAKE_VASTLEGGEN, foutmeldingTextUitJobContext.toString());

					if (intakeMelding.getAantalClienten() == null)
					{
						intakeMelding.setAantalClienten(String.valueOf(clientAfspraken.size()));
					}
					else
					{
						intakeMelding.setAantalClienten(String.format("%s,%s", intakeMelding.getAantalClienten(), clientAfspraken.size()));
					}

					IntHolder aantalExtraDagen = new IntHolder();
					List<VrijSlot> vrijeSloten = getVrijeSloten(clientAfspraken.size(), aantalExtraDagen, intakeMelding);

					if (intakeMelding.getAantalVrijesloten() == null)
					{
						intakeMelding.setAantalVrijesloten(String.valueOf(vrijeSloten.size()));
					}
					else
					{
						intakeMelding.setAantalVrijesloten(String.format("%s,%s", intakeMelding.getAantalVrijesloten(), vrijeSloten.size()));
					}
					intakeMelding.setAantalExtraDagen(intakeMelding.getAantalExtraDagen() + aantalExtraDagen.value);

					if (aantalExtraDagen.value > 0)
					{
						intakeMelding.setLevel(Level.WARNING);
					}
					if (vrijeSloten.size() < clientAfspraken.size())
					{
						intakeMelding.setLevel(Level.ERROR);
					}
					StringBuilder planningResultaat = new StringBuilder();

					innerExecutionContext.put(IntakeAfsprakenMakenConstants.ALLE_INTAKES_VERWERKT, Boolean.TRUE);

					if (ronde == 0)
					{
						clienten = planIntakeAfsprakenService.planIntakeAfspraken(clientAfspraken, vrijeSloten, planningResultaat).iterator();
					}
					else
					{
						clienten = planIntakeAfsprakenService.planIntakeAfspraken(clientAfspraken, vrijeSloten, planningResultaat, maximumRetrySecondsSpend).iterator();
					}
					intakeMelding.setAantalRondes(ronde);
					intakeMelding.setPlannerResultaat(planningResultaat.toString());
				}
				finally
				{
					if (unbindSessionFromThread)
					{
						TransactionSynchronizationManager.unbindResource(sessionFactory);
						SessionFactoryUtils.closeSession(hibernateSession);
					}
				}

			}
		});
	}

	private List<VrijSlot> getVrijeSloten(int aantalGeselecteerdeClienten, IntHolder aantalExtraDagen, IntakeMakenLogEvent intakeMelding)
	{
		Date laatsteEindDatum = (Date) stepExecution.getJobExecution().getExecutionContext().get(IntakeAfsprakenMakenConstants.LAATSTE_EIND_DATUM);
		Date begintijd;
		Date eindtijd;

		DateTime nu = currentDateSupplier.getDateTime();

		Integer ongunstigeUitslagWachtPeriode = preferenceService.getInteger(PreferenceKey.ONGUNSTIGE_UITSLAG_WACHT_PERIODE.name());
		if (ongunstigeUitslagWachtPeriode == null)
		{

			ongunstigeUitslagWachtPeriode = Integer.valueOf(2); 
		}
		Integer intakeafspraakperiode = preferenceService.getInteger(PreferenceKey.INTAKEAFSPRAAKPERIODE.name());
		if (intakeafspraakperiode == null)
		{

			intakeafspraakperiode = Integer.valueOf(14); 
		}

		begintijd = DateUtil.plusWerkdagen(nu, ongunstigeUitslagWachtPeriode).withTimeAtStartOfDay().toDate();

		if (laatsteEindDatum == null)
		{
			eindtijd = nu.plusDays(intakeafspraakperiode).withTimeAtStartOfDay().toDate();
		}
		else
		{
			eindtijd = new DateTime(laatsteEindDatum).plusDays(1).withTimeAtStartOfDay().toDate();
		}

		intakeMelding.setBeginTijd(begintijd);

		List<VrijSlot> vrijeSloten = intakeAfspraakService.getAllVrijeSlotenIntakeafspraakperiode(aantalGeselecteerdeClienten, begintijd, eindtijd, aantalExtraDagen);

		Date newLaatsteEindDatum = new DateTime(eindtijd).plusDays(aantalExtraDagen.value).withTimeAtStartOfDay().toDate();
		intakeMelding.setEindTijd(newLaatsteEindDatum);

		stepExecution.getJobExecution().getExecutionContext().put(IntakeAfsprakenMakenConstants.LAATSTE_EIND_DATUM, newLaatsteEindDatum);

		return vrijeSloten;
	}

	@Override
	public void update(ExecutionContext executionContext) throws ItemStreamException
	{

	}

	@Override
	public void close() throws ItemStreamException
	{

	}

	@BeforeStep
	public void saveStepExecution(StepExecution stepExecution)
	{
		this.stepExecution = stepExecution;
	}

}
