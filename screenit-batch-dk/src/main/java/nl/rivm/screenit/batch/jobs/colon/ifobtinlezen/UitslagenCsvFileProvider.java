
package nl.rivm.screenit.batch.jobs.colon.ifobtinlezen;

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

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.batch.CsvFileProvider;
import nl.rivm.screenit.model.colon.IFobtLaboratorium;
import nl.rivm.screenit.service.InstellingService;

import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import au.com.bytecode.opencsv.CSVReader;

public class UitslagenCsvFileProvider implements CsvFileProvider
{
	
	private static final Logger LOG = LoggerFactory.getLogger(UitslagenCsvFileProvider.class);

	@Autowired
	private InstellingService instellingService;

	private String uitslagenFileLocation;

	private boolean fromClasspath;

	private String separator;

	@Override
	public Iterator<CSVReader> getReaders() throws IllegalStateException, IOException
	{
		List<CSVReader> readers = new ArrayList<>();

		if (fromClasspath)
		{
			String path = uitslagenFileLocation;
			path = UitslagenCsvFileProvider.class.getResource(uitslagenFileLocation).getFile();
			readers.add(new CSVReader(new FileReader(new File(path)), '	'));
		}
		else
		{
			List<IFobtLaboratorium> labs = instellingService.getActieveInstellingen(IFobtLaboratorium.class);
			File dir = new File(uitslagenFileLocation);
			for (IFobtLaboratorium ifobtLaboratorium : labs)
			{
				if (ifobtLaboratorium.getLabId() == null)
				{
					throw new IllegalStateException("Geen Lab ID ingesteld voor lab: " + ifobtLaboratorium.getNaam());
				}
			}

			if (!dir.exists() || !dir.isDirectory())
			{
				throw new IllegalStateException("Uitslagen bestandslocatie niet gevonden of geen directory: " + uitslagenFileLocation);
			}

			for (File file : dir.listFiles())
			{
				if (!file.isDirectory() && !file.getName().startsWith("."))
				{
					if (isNotEmpytFile(file))
					{
						CSVFileReader reader = new CSVFileReader(new FileReader(file), separator.charAt(0), file.getName());

						readers.add(reader);
						LOG.info("Reader toegevoegd voor file: " + file.getPath());
					}
					else
					{
						LOG.warn("File is leeg. Geen reader toegevoegd (overgeslagen) voor file: " + file.getPath());
					}
				}
			}
			if (readers.size() == 0)
			{
				LOG.warn("Geen bestanden met uitslagen gevonden in: " + uitslagenFileLocation);
			}
			String seperator = System.getProperty("file.separator");
			String archiefLoaction = uitslagenFileLocation + seperator + "archief";

			File archiefDir = new File(archiefLoaction);

			if (!archiefDir.exists())
			{
				FileUtils.forceMkdir(archiefDir);
			}
		}

		return new CSVReaderIterator(readers.iterator());
	}

	private boolean isNotEmpytFile(File file)
	{
		boolean isNotEmpty = false;
		try
		{

			isNotEmpty = file.length() > 4096 || !FileUtils.readFileToString(file).trim().isEmpty();
		}
		catch (IOException e)
		{
			LOG.warn("Probleem bij bepalen of bestand leeg is", e);
		}
		return isNotEmpty;
	}

	public void setUitslagenFileLocation(String uitslagenFileLocation)
	{
		this.uitslagenFileLocation = uitslagenFileLocation;
	}

	public void setSeparator(String separator)
	{
		this.separator = separator;
	}

	public void setFromClasspath(boolean fromClasspath)
	{
		this.fromClasspath = fromClasspath;
	}

}
