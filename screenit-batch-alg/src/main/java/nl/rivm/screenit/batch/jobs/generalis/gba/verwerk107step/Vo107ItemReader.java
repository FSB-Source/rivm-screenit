
package nl.rivm.screenit.batch.jobs.generalis.gba.verwerk107step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Calendar;
import java.util.Iterator;
import java.util.UUID;

import nl.rivm.screenit.batch.jobs.generalis.gba.GbaConstants;
import nl.rivm.screenit.batch.jobs.generalis.gba.verwerk107step.IVo107Provider.Vo107File;
import nl.rivm.screenit.model.gba.GbaFile;
import nl.rivm.screenit.model.gba.GbaFoutCategorie;
import nl.rivm.screenit.model.gba.GbaFoutRegel;
import nl.rivm.screenit.model.gba.GbaVerwerkingsLog;
import nl.topicuszorg.gba.vertrouwdverbonden.exceptions.Vo107ParseException;
import nl.topicuszorg.gba.vertrouwdverbonden.model.Vo107Bericht;
import nl.topicuszorg.gba.vertrouwdverbonden.services.VO107Service;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5Session;

import org.slf4j.LoggerFactory;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemReader;
import org.springframework.batch.item.ItemStream;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.beans.factory.annotation.Autowired;

public class Vo107ItemReader implements ItemReader<Vo107Bericht>, ItemStream
{

	private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(Vo107ItemReader.class);

	private static final String OFFSET = "key.offset";

	@Autowired
	private VO107Service vo107Service;

	private GbaVerwerkingsLog verwerkingLog;

	private String voFileStorePath;

	private Iterator<Vo107Bericht> berichtIterator;

	private Vo107File currentFile;

	private Iterator<Vo107File> fileIterator;

	private IVo107Provider vo107Provider;

	private int offset;

	private StepExecution stepExecution;

	private InputStream fileInputStream;

	@Override
	public Vo107Bericht read()
	{

		verwerkingLog = (GbaVerwerkingsLog) stepExecution.getJobExecution().getExecutionContext().get(GbaConstants.RAPPORTAGEKEYGBA);
		try
		{
			if (berichtIterator != null && berichtIterator.hasNext())
			{
				offset++;
				return berichtIterator.next();
			}
			else
			{
				if (fileInputStream != null)
				{
					fileInputStream.close();
				}
				if (currentFile != null)
				{
					currentFile.deleteFile();
				}
				if (fileIterator.hasNext())
				{
					nextInputstream(verwerkingLog);
					return read();
				}
			}
		}
		catch (Vo107ParseException e)
		{
			if (currentFile != null)
			{
				String melding = "Error parsing file " + currentFile.getFilename() + " Melding: " + e.getMessage();
				LOG.error(melding);
				createFoutRegel(verwerkingLog, melding);
			}
			else
			{
				LOG.error("Error parsing Vo107 file", e);
			}
			throw e;
		}
		catch (IOException e)
		{
			LOG.error("Kon bestand niet sluiten", e);
		}
		return null;
	}

	@Override
	public void open(ExecutionContext executionContext) throws ItemStreamException
	{
		berichtIterator = null;
		currentFile = null;
		fileIterator = null;
		offset = 0;

		OpenHibernate5Session.withCommittedTransaction().run(() -> {
			GbaVerwerkingsLog verwerkingsLog = (GbaVerwerkingsLog) stepExecution.getJobExecution().getExecutionContext().get(GbaConstants.RAPPORTAGEKEYGBA);

			fileIterator = vo107Provider.getVo107Files(verwerkingsLog).iterator();
			nextInputstream(verwerkingsLog);

			if (berichtIterator == null)
			{
				LOG.warn("Geen GBA bestanden gevonden");
			}

			if (executionContext.containsKey(OFFSET))
			{
				for (int i = 0; i < executionContext.getInt(OFFSET); i++)
				{
					if (berichtIterator.hasNext())
					{
						berichtIterator.next();
					}
					else if (fileIterator.hasNext())
					{
						nextInputstream(verwerkingsLog);
					}
				}
			}
		});
	}

	private void nextInputstream(GbaVerwerkingsLog verwerkingsLog)
	{
		if (fileIterator.hasNext())
		{
			currentFile = fileIterator.next();

			GbaFile gbaFile = new GbaFile();
			gbaFile.setNaam(currentFile.getFilename());
			gbaFile.setGbaVerwerkingsLog(verwerkingsLog);
			verwerkingsLog.getBestanden().add(gbaFile);

			fileInputStream = saveStream(currentFile, gbaFile);
			berichtIterator = vo107Service.fetchVo107Berichten(fileInputStream, "UTF8", true);
		}
		else
		{
			currentFile = null;
		}
	}

	private InputStream saveStream(Vo107File vo107File, GbaFile gbaFile)
	{
		StringBuilder directory = new StringBuilder();

		directory.append(voFileStorePath);

		Calendar cal = Calendar.getInstance();
		getDailyPath(directory, cal);

		String uniqueFileName = UUID.randomUUID().toString();
		uniqueFileName += "-incoming.vo107";
		File dir = new File(directory.toString());
		dir.mkdirs();
		File file = new File(directory.toString() + System.getProperty("file.separator") + uniqueFileName);

		StringBuilder dbPath = new StringBuilder();
		getDailyPath(dbPath, cal);
		dbPath.append(System.getProperty("file.separator"));
		dbPath.append(uniqueFileName);
		gbaFile.setPath(dbPath.toString());

		LOG.info("Saved file: " + file.getPath());

		try
		{
			file.createNewFile();
			vo107File.saveToFile(file);
			return new FileInputStream(file);
		}
		catch (IOException e)
		{
			LOG.error("Error bij opslaan van vo107 bestand", e);
			createFoutRegel(verwerkingLog, "Error bij opslaan van vo107 bestand");
		}

		return null;
	}

	private void createFoutRegel(GbaVerwerkingsLog gbaVerwerkingsLog, String foutregel)
	{
		GbaFoutRegel gbaFoutRegel = new GbaFoutRegel();
		gbaFoutRegel.setFout(foutregel);
		gbaFoutRegel.setFoutCategorie(GbaFoutCategorie.PROCES);
		gbaFoutRegel.setVerwerkingsLog(gbaVerwerkingsLog);
		gbaVerwerkingsLog.getFouten().add(gbaFoutRegel);
	}

	protected void getDailyPath(StringBuilder directory, Calendar cal)
	{
		directory.append(System.getProperty("file.separator"));
		directory.append(cal.get(Calendar.YEAR));
		directory.append(System.getProperty("file.separator"));
		directory.append(cal.get(Calendar.MONTH) + 1);
		directory.append(System.getProperty("file.separator"));
		directory.append(cal.get(Calendar.DAY_OF_MONTH));
		directory.append(System.getProperty("file.separator"));
	}

	@Override
	public void update(ExecutionContext executionContext) throws ItemStreamException
	{
		executionContext.putInt(OFFSET, offset);
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

	public void setVo107Provider(IVo107Provider vo107Provider)
	{
		this.vo107Provider = vo107Provider;
	}

	public void setVoFileStorePath(String voFileStorePath)
	{
		this.voFileStorePath = voFileStorePath;
	}

}
