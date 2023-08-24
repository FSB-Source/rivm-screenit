package nl.rivm.screenit.batch.jobs.helpers;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.batch.service.BarcodeValiderenService;
import nl.rivm.screenit.batch.service.WebserviceInpakcentrumOpzettenService;
import nl.rivm.screenit.model.algemeen.KoppelData;
import nl.rivm.screenit.model.enums.JobStartParameter;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.logging.LogEvent;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemStream;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.SessionHolder;
import org.springframework.transaction.support.TransactionSynchronizationManager;
import org.tempuri.IUpload;
import org.tempuri.UploadRequest;

import generated.KOPPELDATA;

public abstract class BaseKoppelReader implements ItemStream
{
	private static final Logger LOG = LoggerFactory.getLogger(BaseKoppelReader.class);

	@Autowired
	protected SessionFactory sessionFactory;

	@Autowired
	private BarcodeValiderenService validerenService;

	@Autowired
	private WebserviceInpakcentrumOpzettenService webserviceOpzettenService;

	protected Session hibernateSession;

	private StepExecution stepExecution;

	protected boolean unbindSessionFromThread = false;

	protected Iterator<KOPPELDATA.VERZONDENUITNODIGING> koppeldata;

	@Override
	public final void open(ExecutionContext executionContext) throws ItemStreamException
	{
		hibernateSession = sessionFactory.openSession();
		doOpen(executionContext);
	}

	public KOPPELDATA.VERZONDENUITNODIGING read()
	{
		if (koppeldata.hasNext())
		{
			return koppeldata.next();
		}
		return null;
	}

	@Override
	public final void update(ExecutionContext executionContext) throws ItemStreamException
	{
		doUpdate(executionContext);
	}

	@Override
	public final void close() throws ItemStreamException
	{
		try
		{
			doClose();
		}
		finally
		{
			hibernateSession.getSession().close();
		}
	}

	protected final SessionFactory getSessionFactory()
	{
		return sessionFactory;
	}

	protected final Session getHibernateSession()
	{
		return hibernateSession.getSession();
	}

	protected final StepExecution getStepExecution()
	{
		return stepExecution;
	}

	protected void doOpen(ExecutionContext executionContext) throws ItemStreamException
	{
		LogEvent logEvent = (LogEvent) getStepExecution().getJobExecution().getExecutionContext()
			.get(getKoppelenConstant());

		List<String> semantischeFoutmeldingen = new ArrayList<>();

		try
		{
			List<KOPPELDATA.VERZONDENUITNODIGING> koppeldataLijst = getKoppeldataLijst();
			semantischeFoutmeldingen = validerenService.voerSemantischeValiatieUit(koppeldataLijst);

			if (!semantischeFoutmeldingen.isEmpty())
			{
				voorkomVerwerkingKoppeldata(koppeldataLijst);
			}

			controleerOfAlleenValidatieNodigIs(logEvent);

			boolean versturenSuccesvol = verstuurSemantischeFoutmeldingen(semantischeFoutmeldingen);

			if (!versturenSuccesvol)
			{
				voorkomVerwerkingKoppeldata(koppeldataLijst);
			}
			koppeldata = koppeldataLijst.iterator();
		}
		catch (JAXBException | IOException e)
		{
			logEvent.setLevel(Level.ERROR);
			logEvent.setMelding("Er is een probleem opgetreden met een webservice, neem contact op met de helpdesk.");
			throw new ItemStreamException(e);
		}
		finally
		{
			LogEvent eindEvent = new LogEvent();
			eindEvent.setLevel(Level.INFO);

			if (semantischeFoutmeldingen.size() > 0)
			{
				LOG.warn("Fouten gevonden: #" + semantischeFoutmeldingen.size());
				eindEvent.setLevel(Level.ERROR);
				eindEvent.setMelding("De validatie heeft fouten gevonden en teruggekoppeld, Aantal fouten: #" + semantischeFoutmeldingen.size());
			}

			logEindValidatie(eindEvent);

			unbindSessionIfPossible();
		}
	}

	protected void doUpdate(ExecutionContext executionContext)
	{

	}

	protected void doClose()
	{

	}

	@BeforeStep
	public void saveStepExecution(StepExecution stepExecution)
	{
		this.stepExecution = stepExecution;
	}

	protected List<KOPPELDATA.VERZONDENUITNODIGING> getKoppeldataLijst() throws JAXBException
	{
		JAXBContext jaxbContext = JAXBContext.newInstance(KOPPELDATA.class);
		Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();

		if (!TransactionSynchronizationManager.hasResource(sessionFactory))
		{
			TransactionSynchronizationManager.bindResource(sessionFactory, new SessionHolder(hibernateSession));
			unbindSessionFromThread = true;
		}
		KoppelData koppelData = getKoppelData();

		return ((KOPPELDATA) unmarshaller.unmarshal(new StringReader(koppelData.getXmlBericht()))).getVERZONDENUITNODIGING();
	}

	private KoppelData getKoppelData()
	{
		Long id = getStepExecution().getJobParameters().getLong(JobStartParameter.KOPPEL_XML.name());
		return getHibernateSession().get(KoppelData.class, id);
	}

	protected boolean verstuurSemantischeFoutmeldingen(List<String> foutmeldingen) throws IOException
	{
		LOG.info("Webservice opzetten");

		IUpload upload = webserviceOpzettenService.initialiseerWebserviceInpakcentrum();

		LOG.info("Start versturen, semantische foutmeldingen size: " + foutmeldingen.size());
		boolean result;

		UploadRequest uploadRequest = new UploadRequest();
		uploadRequest.setStream(maakCsvStream(foutmeldingen).toByteArray());
		result = upload
			.upload(uploadRequest, "csv", getFileNaam() + ".csv",
				foutmeldingen.size())
			.isUploadSucceeded();

		result = upload.getReady(result);

		if (!result)
		{
			LogEvent logEvent = new LogEvent();
			logEvent.setLevel(Level.ERROR);
			logEvent.setMelding("Het bestand met de semantische meldingen is niet succesvol naar het inpakcentrum verstuurd");

			logKoppelFout(logEvent);
		}

		LOG.info("Alles verstuurd met resultaat: " + result);
		return result;
	}

	private String getFileNaam()
	{
		KoppelData koppelData = getKoppelData();
		String fileNaamVoorValidatie = koppelData.getFilename().replaceAll("mergedata", "validatie");
		String[] filenameParts = fileNaamVoorValidatie.split("\\.");
		return filenameParts[0];
	}

	private ByteArrayOutputStream maakCsvStream(List<String> foutmeldingen) throws IOException
	{
		ByteArrayOutputStream csvStream = new ByteArrayOutputStream();

		for (String foutmelding : foutmeldingen)
		{
			csvStream.write(foutmelding.getBytes());
			csvStream.write(System.lineSeparator().getBytes());
		}

		return csvStream;
	}

	protected void controleerOfAlleenValidatieNodigIs(LogEvent logEvent)
	{
		boolean onlyValidation = Boolean.parseBoolean(getStepExecution().getJobParameters().getString(Constants.ALLEEN_VALIDATIE));

		if (onlyValidation)
		{
			logEvent.setLevel(Level.INFO);
			logEvent.setMelding("Webservice alleen benaderd voor validatie");
		}
	}

	protected void voorkomVerwerkingKoppeldata(List<KOPPELDATA.VERZONDENUITNODIGING> koppeldataLijst)
	{
		koppeldataLijst.clear();
	}

	protected abstract void logEindValidatie(LogEvent eindEvent);

	protected abstract void logKoppelFout(LogEvent logEvent);

	protected abstract String getAfkortingBvo();

	protected abstract String getKoppelenConstant();

	protected void unbindSessionIfPossible()
	{
		if (unbindSessionFromThread)
		{
			TransactionSynchronizationManager.unbindResource(sessionFactory);
			unbindSessionFromThread = false;
		}
	}
}
