package nl.rivm.screenit.batch.jobs.generalis.coordinaten;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;

import nl.rivm.screenit.batch.service.impl.PostcodeNlRestService;
import nl.rivm.screenit.batch.service.impl.PostcodeNlProductCode;
import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemReader;
import org.springframework.batch.item.ItemStream;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.beans.factory.annotation.Autowired;

public class PostcodeNlDataReader implements ItemReader<String>, ItemStream
{
	private static final Logger LOGGER = LoggerFactory.getLogger(PostcodeNlDataReader.class);

	private BufferedReader reader;

	private boolean ready = false;

	private String productCode;

	private File tempFile;

	@Autowired
	private PostcodeNlRestService postcodeNlRestService;

	@Override
	public String read() throws IOException
	{
		String nextLine = null;
		try
		{
			if (reader != null)
			{
				nextLine = reader.readLine();
				if (nextLine == null)
				{
					ready = true;
				}
			}
		}
		catch (IOException e)
		{
			LOGGER.error("Error bij starten reader: " + e.getMessage());
			throw e;
		}
		return nextLine;
	}

	@Override
	public void open(ExecutionContext executionContext)
	{
		LOGGER.info("Inputstream voor " + productCode + " wordt geopend en ingelezen.");
		try
		{
			InputStream inputStream = postcodeNlRestService.getDelivery(PostcodeNlProductCode.valueOf(productCode));
			if (inputStream != null)
			{
				tempFile = File.createTempFile(productCode, ".zip");
				FileUtils.copyInputStreamToFile(inputStream, tempFile);
				reader = new BufferedReader(new FileReader(tempFile));
			}
			else
			{
				LOGGER.info("Geen input voor " + productCode + " gevonden");
			}
		}
		catch (IOException e)
		{
			close();
			LOGGER.error("Error bij openen inputstream: " + e.getMessage());
			throw new ItemStreamException(e);
		}
	}

	@Override
	public void update(ExecutionContext executionContext) throws ItemStreamException
	{

	}

	@Override
	public void close() throws ItemStreamException
	{
		if (reader != null)
		{
			try
			{
				reader.close();
				FileUtils.deleteQuietly(tempFile);
				reader = null;
			}
			catch (IOException e)
			{
				LOGGER.error("Error bij sluiten reader: " + e.getMessage());
				throw new ItemStreamException(e);
			}
		}
	}

	public String getProductCode()
	{
		return productCode;
	}

	public void setProductCode(String productCode)
	{
		this.productCode = productCode;
	}

}
