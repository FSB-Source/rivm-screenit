package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.io.FileReader;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import nl.topicuszorg.util.collections.CollectionUtils;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import au.com.bytecode.opencsv.CSVReader;

public class ClientenBestandVerwerkingContext
{

	private static final Logger LOG = LoggerFactory.getLogger(ClientenBestandVerwerkingContext.class);

	private static final String BSN = "bsn";

	private static final String GEBOORTEDATUM = "geboortedatum";

	private CSVReader reader = null;

	private String[] huidigeLine = null;

	private int bsnColumn = -1;

	private int geboortedatumColumn = -1;

	private int regelnummer;

	public ClientenBestandVerwerkingContext(File file) throws Exception
	{
		getMostUsefullReader(file);
		bepaalVolgordeHeaders();
		regelnummer = 1;

	}

	private void getMostUsefullReader(File file) throws Exception
	{
		CSVReader testReader = null;
		try
		{
			testReader = new CSVReader(new FileReader(file), ',');
			huidigeLine = testReader.readNext();
			if (huidigeLine != null && Arrays.asList(huidigeLine).size() >= 2)
			{
				reader = testReader;
			}
			else
			{
				testReader.close();
				huidigeLine = null;
				testReader = new CSVReader(new FileReader(file), ';');
				huidigeLine = testReader.readNext();
				if (huidigeLine != null && Arrays.asList(huidigeLine).size() >= 2)
				{
					reader = testReader;
				}
				else
				{
					testReader.close();
					testReader = null;
					throw new IllegalStateException("File die is aangeleverd voldoet niet aan het juiste formaat");
				}
			}
		}
		catch (Exception e)
		{
			if (testReader != null)
			{
				try
				{

					testReader.close();
				}
				catch (IOException e1)
				{
					LOG.error("Van het projectbestand kan de reader niet worden gesloten", e);
					throw e;
				}
			}
		}
	}

	private void bepaalVolgordeHeaders()
	{
		List<String> headers = getHuidigeRegel();
		if (CollectionUtils.isNotEmpty(headers))
		{
			for (String header : headers)
			{
				String geformateerdeHeader = header.toLowerCase().trim();
				if (BSN.equals(geformateerdeHeader))
				{
					bsnColumn = headers.indexOf(header);
				}
				else if (GEBOORTEDATUM.equals(geformateerdeHeader))
				{
					geboortedatumColumn = headers.indexOf(header);
				}
			}
			if (bsnColumn == -1 || geboortedatumColumn == -1)
			{
				throw new IllegalStateException("Geen header met bsn of geboortedatum gevonden in het bestand.");
			}
		}
	}

	public Date getGeboortedatumVanHuidigeRegel() throws IllegalStateException
	{
		String value = huidigeLine[geboortedatumColumn];
		if (StringUtils.isNotBlank(value))
		{
			SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy");
			try
			{
				return format.parse(value);
			}
			catch (ParseException e)
			{
				try
				{
					format = new SimpleDateFormat("d-M-yyyy");
					return format.parse(value);
				}
				catch (ParseException e1)
				{
					throw new IllegalStateException("Datum kon niet worden herkend gebruik het format, 1-1-2015 of 01-01-2015");
				}
			}
		}
		throw new IllegalStateException("Datum kon niet worden herkend gebruik het format, 1-1-2015 of 01-01-2015");
	}

	public String getBsnVanHuidigeRegel()
	{
		return huidigeLine[bsnColumn].trim();
	}

	public List<String> getHuidigeRegel()
	{
		return Arrays.asList(huidigeLine);
	}

	public boolean isErEenNieuweRegel() throws IOException
	{
		huidigeLine = reader.readNext();
		if (huidigeLine != null)
		{
			regelnummer++;
			return true;
		}
		return false;
	}

	public int getRegelnummer()
	{
		return regelnummer;
	}

	public void close()
	{
		if (reader != null)
		{
			try
			{
				reader.close();
			}
			catch (IOException e)
			{
				LOG.error("Fout bij sluiten van reader", e);
			}
		}
	}
}
