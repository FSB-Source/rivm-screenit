package nl.rivm.screenit.batch.jobs.colon.intake.afsprakenmakenstep;

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

import java.time.LocalDate;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.colon.intake.IntakeAfsprakenMakenConstants;
import nl.rivm.screenit.batch.model.ClientAfspraak;
import nl.rivm.screenit.batch.service.IntakeAfpraakService;
import nl.rivm.screenit.batch.service.PlanIntakeAfsprakenService;
import nl.rivm.screenit.model.colon.dto.VrijSlot;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.logging.IntakeMakenLogEvent;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
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
import org.springframework.stereotype.Component;
import org.springframework.transaction.support.TransactionSynchronizationManager;

@Component
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
	@Qualifier("maximumSecondsSpend")
	private Long maximumSecondsSpend = 120L;

	@Autowired
	@Qualifier("aantalSecondenPerClient")
	private Long aantalSecondenPerClient = 6L;

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

			AtomicInteger aantalExtraDagen = new AtomicInteger();
			List<VrijSlot> vrijeSloten = getVrijeSloten(clientAfspraken.size(), aantalExtraDagen, intakeMelding);

			if (intakeMelding.getAantalVrijesloten() == null)
			{
				intakeMelding.setAantalVrijesloten(String.valueOf(vrijeSloten.size()));
			}
			else
			{
				intakeMelding.setAantalVrijesloten(String.format("%s,%s", intakeMelding.getAantalVrijesloten(), vrijeSloten.size()));
			}
			intakeMelding.setAantalExtraDagen(intakeMelding.getAantalExtraDagen() + aantalExtraDagen.get());

			if (aantalExtraDagen.get() > 0)
			{
				intakeMelding.setLevel(Level.WARNING);
			}
			if (vrijeSloten.size() < clientAfspraken.size())
			{
				intakeMelding.setLevel(Level.ERROR);
			}
			StringBuilder planningResultaat = new StringBuilder();

			innerExecutionContext.put(IntakeAfsprakenMakenConstants.ALLE_INTAKES_VERWERKT, Boolean.TRUE);
			long maximaleTijd = aantalSecondenPerClient * clientAfspraken.size();
			if (ronde == 0)
			{
				if (maximaleTijd > maximumSecondsSpend)
				{
					maximaleTijd = maximumSecondsSpend;
				}
				clienten = planIntakeAfsprakenService.planIntakeAfspraken(clientAfspraken, vrijeSloten, planningResultaat, maximaleTijd).iterator();
			}
			else
			{
				if (maximaleTijd > maximumRetrySecondsSpend)
				{
					maximaleTijd = maximumRetrySecondsSpend;
				}
				clienten = planIntakeAfsprakenService.planIntakeAfspraken(clientAfspraken, vrijeSloten, planningResultaat, maximaleTijd).iterator();
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

	private List<VrijSlot> getVrijeSloten(int aantalGeselecteerdeClienten, AtomicInteger aantalExtraDagen, IntakeMakenLogEvent intakeMelding)
	{
		var laatsteEindDatum = (Date) stepExecution.getJobExecution().getExecutionContext().get(IntakeAfsprakenMakenConstants.LAATSTE_EIND_DATUM);
		LocalDate eindDatum;

		var vandaag = currentDateSupplier.getLocalDate();

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

		var beginDatum = DateUtil.plusWerkdagen(vandaag, ongunstigeUitslagWachtPeriode);

		if (laatsteEindDatum == null)
		{
			eindDatum = vandaag.plusDays(intakeafspraakperiode);
		}
		else
		{
			eindDatum = DateUtil.toLocalDate(laatsteEindDatum).plusDays(1);
		}

		intakeMelding.setBeginTijd(DateUtil.toUtilDate(beginDatum));

		List<VrijSlot> vrijeSloten = intakeAfspraakService.getAllVrijeSlotenIntakeafspraakperiode(aantalGeselecteerdeClienten, beginDatum, eindDatum, aantalExtraDagen);

		Date newLaatsteEindDatum = DateUtil.toUtilDate(eindDatum.plusDays(aantalExtraDagen.get()));
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
