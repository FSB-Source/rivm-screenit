package nl.rivm.screenit.batch.jobs.generalis.coordinaten;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.service.impl.PostcodeNlRestService;
import nl.rivm.screenit.model.PostcodeNlProductCode;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5Session;

import org.apache.commons.io.FileUtils;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemReader;
import org.springframework.batch.item.ItemStream;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.beans.factory.annotation.Autowired;

@Slf4j
public class PostcodeNlDataReader implements ItemReader<String>, ItemStream
{

	@Autowired
	private PostcodeNlRestService postcodeNlRestService;

	private BufferedReader reader;

	@Getter
	@Setter
	private String productCode;

	private File tempFile;

	@Override
	public String read() throws IOException
	{
		String nextLine = null;
		try
		{
			if (reader != null)
			{
				nextLine = reader.readLine();
			}
		}
		catch (IOException e)
		{
			LOG.error("Error bij starten reader: " + e.getMessage());
			throw e;
		}
		return nextLine;
	}

	@Override
	public void open(ExecutionContext executionContext)
	{
		LOG.info("Inputstream voor " + productCode + " wordt geopend en ingelezen.");
		OpenHibernate5Session.withoutTransaction().run(() ->
		{
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
					LOG.info("Geen input voor " + productCode + " gevonden");
				}
			}
			catch (IOException e)
			{
				close();
				LOG.error("Error bij openen inputstream: " + e.getMessage());
				throw new ItemStreamException(e);
			}

		});
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
				LOG.error("Error bij sluiten reader: " + e.getMessage());
				throw new ItemStreamException(e);
			}
		}
	}

}
