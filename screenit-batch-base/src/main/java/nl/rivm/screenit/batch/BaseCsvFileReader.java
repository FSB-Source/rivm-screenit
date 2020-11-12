
package nl.rivm.screenit.batch;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.io.Closeable;
import java.io.IOException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.batch.jobs.BatchConstants;
import nl.rivm.screenit.model.enums.Level;

import org.apache.commons.lang.StringUtils;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemReader;
import org.springframework.batch.item.ItemStream;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.SessionHolder;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import au.com.bytecode.opencsv.CSVReader;

public abstract class BaseCsvFileReader<T> implements ItemReader<T>, ItemStream
{

	private static final Logger LOG = LoggerFactory.getLogger(BaseCsvFileReader.class);

	@Autowired
	private SessionFactory sessionFactory;

	private CsvFileProvider cvsFileProvider;

	private Iterator<CSVReader> readers;

	private CSVReader current;

	private Iterator<T> currentResults;

	private JobExecution jobExecution;

	public static final String RAPPORTAGEKEYHASMOREFILES = "key.hasmorefiles";

	public static final String RAPPORTAGEKEYHASMORERESULTS = "key.hasmoreresults";

	public static final String RAPPORTAGEKEYFOUTINBESTAND = "key.foutinbestand";

	@Override
	public T read() throws IOException, ParseException
	{
		String bestandsNaam = "";
		try
		{
			if (readers.hasNext() && (current == null || current != null && !currentResults.hasNext()))
			{
				bestandsNaam = setNextReaderToCurrentAndReturnBestandsNaam();
				currentResults = getResultsFromCurrent(bestandsNaam);

				if (!currentResults.hasNext())
				{
					LOG.warn("Bestand " + bestandsNaam + " is leeg. Door met het volgende bestand.");
					return read();
				}
			}
			if (currentResults != null && currentResults.hasNext())
			{
				T result = currentResults.next();
				getExecutionContext().put(RAPPORTAGEKEYHASMOREFILES, readers.hasNext());
				getExecutionContext().put(RAPPORTAGEKEYHASMORERESULTS, currentResults.hasNext());
				return result;
			}
		}
		catch (Exception e)
		{
			crashMelding("De job heeft onsuccesvol gedraaid, neem contact op met de helpdesk.", e);
			throw e;
		}
		return null;
	}

	private void crashMelding(String melding, Exception e)
	{
		LOG.error(melding, e);
		String huidigeMelding = null;
		if (getExecutionContext().containsKey(BatchConstants.MELDING))
		{
			huidigeMelding = getExecutionContext().getString(BatchConstants.MELDING);
		}
		if (huidigeMelding == null)
		{
			huidigeMelding = melding;
		}
		else if (!huidigeMelding.contains(melding))
		{
			huidigeMelding += ";" + melding;
		}
		getExecutionContext().put(BatchConstants.MELDING, huidigeMelding);
		getExecutionContext().put(BatchConstants.LEVEL, Level.ERROR);
	}

	protected ExecutionContext getExecutionContext()
	{
		return jobExecution.getExecutionContext();
	}

	@BeforeStep
	public void saveStepExecution(StepExecution stepExecution)
	{
		this.jobExecution = stepExecution.getJobExecution();
	}

	protected abstract T parseLine(String[] line, int regelnummer, String bestandsNaam) throws ParseException, IllegalStateException;

	protected static String nullCheckColumn(String waarde) throws ParseException
	{
		if (StringUtils.isNotBlank(waarde))
		{
			return waarde;
		}
		else
		{
			throw new IllegalStateException("Syntax is incorrect, verplicht veld is leeg.");
		}
	}

	private String setNextReaderToCurrentAndReturnBestandsNaam() throws IOException
	{
		String bestandsNaam = "";
		if (readers.hasNext())
		{
			if (current != null)
			{
				current.close();
			}
			current = readers.next();
			if (current instanceof CsvFileProvider.CSVFileReader)
			{
				LOG.info("Parsen van regels in bestand " + ((CsvFileProvider.CSVFileReader) current).getFileNaam());
				CsvFileProvider.CSVFileReader currentReader = (CsvFileProvider.CSVFileReader) current;
				bestandsNaam = currentReader.getFileNaam();
			}
		}
		return bestandsNaam;
	}

	private Iterator<T> getResultsFromCurrent(String bestandsNaam) throws IllegalStateException, IOException
	{
		int regelnummer = 0;
		boolean eersteError = true;
		List<T> resultaten = new ArrayList<>();
		if (current != null)
		{
			String[] line = null;
			while ((line = current.readNext()) != null)
			{
				regelnummer++;
				try
				{
					T result = parseLine(line, regelnummer, bestandsNaam);
					if (result == null)
					{
						break;
					}
					resultaten.add(result);
				}
				catch (IllegalStateException | ParseException e)
				{
					LOG.warn("Foutive formaat " + bestandsNaam + " regel " + regelnummer, e);
					String foutiveFormat = "";
					ExecutionContext context = getExecutionContext();
					if (context.containsKey(RAPPORTAGEKEYFOUTINBESTAND))
					{
						foutiveFormat = context.getString(RAPPORTAGEKEYFOUTINBESTAND);
					}
					if (eersteError)
					{
						if (StringUtils.isNotBlank(foutiveFormat))
						{
							foutiveFormat += "<br>";
						}
						foutiveFormat += "Format fout in bestand (regel(s) overgeslagen): " + bestandsNaam + " regelnummer(s) ";
						eersteError = false;
					}
					else
					{
						foutiveFormat += ",";
					}
					foutiveFormat += regelnummer;
					context.putString(RAPPORTAGEKEYFOUTINBESTAND, foutiveFormat);
				}
			}
		}
		return resultaten.iterator();
	}

	@Override
	public void open(ExecutionContext executionContext) throws ItemStreamException
	{

		current = null;
		currentResults = null;
		readers = null;

		Session hibernateSession = null;
		try
		{
			if (sessionFactory != null)
			{
				hibernateSession = sessionFactory.openSession();
				TransactionSynchronizationManager.bindResource(sessionFactory, new SessionHolder(hibernateSession));
			}

			readers = cvsFileProvider.getReaders();
		}
		catch (IllegalStateException e)
		{
			crashMelding(e.getMessage(), e);
			throw e;
		}
		catch (Exception e)
		{
			crashMelding("Er is een probleem bij openen van bestand.", e);
			throw new ItemStreamException(e);
		}
		finally
		{
			if (sessionFactory != null)
			{
				TransactionSynchronizationManager.unbindResource(sessionFactory);
				if (hibernateSession != null)
				{
					hibernateSession.close();
				}
			}
		}
	}

	@Override
	public void update(ExecutionContext executionContext) throws ItemStreamException
	{

	}

	@Override
	public void close() throws ItemStreamException
	{
		try
		{
			if (readers instanceof Closeable)
			{
				Closeable closeable = (Closeable) readers;
				closeable.close();
			}
		}
		catch (Exception e)
		{
			throw new ItemStreamException(e);
		}
	}

	public void setCsvFileProvider(CsvFileProvider cvsFileProvider)
	{
		this.cvsFileProvider = cvsFileProvider;
	}
}
