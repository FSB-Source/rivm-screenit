package nl.rivm.screenit.batch.jobs.generalis.gba.verwerk107step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.time.LocalDate;
import java.util.Iterator;
import java.util.UUID;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.generalis.gba.GbaConstants;
import nl.rivm.screenit.batch.jobs.generalis.gba.verwerk107step.IVo107Provider.Vo107File;
import nl.rivm.screenit.model.gba.GbaFile;
import nl.rivm.screenit.model.gba.GbaFoutCategorie;
import nl.rivm.screenit.model.gba.GbaFoutRegel;
import nl.rivm.screenit.model.gba.GbaVerwerkingsLog;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.ZipUtil;
import nl.topicuszorg.gba.vertrouwdverbonden.exceptions.Vo107ParseException;
import nl.topicuszorg.gba.vertrouwdverbonden.model.Vo107Bericht;
import nl.topicuszorg.gba.vertrouwdverbonden.services.VO107Service;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5Session;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemReader;
import org.springframework.batch.item.ItemStream;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
public class Vo107ItemReader implements ItemReader<Vo107Bericht>, ItemStream
{

	private static final String OFFSET = "key.offset";

	private static final String ZIP_FILE_EXTENSION = ".zip";

	@Autowired
	private VO107Service vo107Service;

	@Autowired
	private FileService fileService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

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
				var isOngeldigBestand = !StringUtils.isAsciiPrintable(e.getMessage());
				var melding = "Leesfout in GBA-bestand " + currentFile.getFilename() + " door: " + (isOngeldigBestand ? e.getMessage() : "Ongeldig bestand");
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
	@Transactional(propagation = Propagation.NEVER)
	public void open(ExecutionContext executionContext) throws ItemStreamException
	{
		berichtIterator = null;
		currentFile = null;
		fileIterator = null;
		offset = 0;

		OpenHibernate5Session.withCommittedTransaction().run(() ->
		{
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
		deleteCurrentFileIfExists();

		if (fileIterator.hasNext())
		{
			currentFile = fileIterator.next();

			try
			{
				var tempGbaFile = getGbaTempFile(currentFile);
				var relativeFileStoreLocation = saveToFilestoreAsZip(currentFile);

				var gbaFile = new GbaFile();
				gbaFile.setNaam(FilenameUtils.removeExtension(currentFile.getFilename()) + ZIP_FILE_EXTENSION);
				gbaFile.setGbaVerwerkingsLog(verwerkingsLog);
				gbaFile.setPath(relativeFileStoreLocation);
				verwerkingsLog.getBestanden().add(gbaFile);

				fileInputStream = Files.newInputStream(tempGbaFile.toPath());
				berichtIterator = vo107Service.fetchVo107Berichten(fileInputStream, "UTF8", true);
			}
			catch (IOException e)
			{
				LOG.error("Error bij ophalen van vo107 bestand", e);
				createFoutRegel(verwerkingLog, "Error bij ophalen van vo107 bestand");
			}
		}
		else
		{
			currentFile = null;
		}
	}

	private File getGbaTempFile(Vo107File vo107File) throws IOException
	{
		var tempVo107File = File.createTempFile(FilenameUtils.removeExtension(vo107File.getFilename()), ".vo107");
		vo107File.saveToTempFile(tempVo107File);
		return tempVo107File;
	}

	private String saveToFilestoreAsZip(Vo107File vo107File)
	{
		var fileName = UUID.randomUUID() + "-incoming" + ZIP_FILE_EXTENSION;
		var datePath = dateToPath(dateSupplier.getLocalDate());

		var relativeFilestorePath = datePath + fileName;
		var fileStorePath = voFileStorePath + System.getProperty("file.separator") + relativeFilestorePath;

		File tempZipFile = null;

		try
		{
			tempZipFile = File.createTempFile(fileName, ZIP_FILE_EXTENSION);
			ZipUtil.zipFileOrDirectory(vo107File.getTempFile().getPath(), tempZipFile.getPath(), true);

			fileService.save(fileStorePath, tempZipFile);

			LOG.info("vo107 bestand {} opgeslagen onder {}", vo107File.getFilename(), fileStorePath);
		}
		catch (IOException e)
		{
			LOG.error("Error bij zippen en opslaan van vo107 bestand", e);
			createFoutRegel(verwerkingLog, "Error bij zippen en opslaan van vo107 bestand");
		}
		finally
		{
			if (tempZipFile != null && tempZipFile.exists())
			{
				try
				{
					FileUtils.delete(tempZipFile);
				}
				catch (IOException e)
				{
					LOG.error("Fout bij verwijderen tijdelijk ZIP bestand");
				}
			}
		}
		return relativeFilestorePath;
	}

	private void createFoutRegel(GbaVerwerkingsLog gbaVerwerkingsLog, String foutregel)
	{
		GbaFoutRegel gbaFoutRegel = new GbaFoutRegel();
		gbaFoutRegel.setFout(foutregel);
		gbaFoutRegel.setFoutCategorie(GbaFoutCategorie.PROCES);
		gbaFoutRegel.setVerwerkingsLog(gbaVerwerkingsLog);
		gbaVerwerkingsLog.getFouten().add(gbaFoutRegel);
	}

	protected String dateToPath(LocalDate date)
	{
		var separator = System.getProperty("file.separator");
		return separator + date.getYear() + separator + date.getMonthValue() + separator + date.getDayOfMonth() + separator;
	}

	private void deleteCurrentFileIfExists()
	{
		if (currentFile != null && currentFile.getTempFile() != null && currentFile.getTempFile().exists())
		{
			try
			{
				FileUtils.delete(currentFile.getTempFile());
			}
			catch (Exception e)
			{
				LOG.error("Fout bij verwijderen van tijdelijke Vo107 bestand {}", currentFile.getFilename(), e);
			}
		}
	}

	@Override
	public void update(ExecutionContext executionContext) throws ItemStreamException
	{
		executionContext.putInt(OFFSET, offset);
	}

	@Override
	public void close() throws ItemStreamException
	{
		deleteCurrentFileIfExists();
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
