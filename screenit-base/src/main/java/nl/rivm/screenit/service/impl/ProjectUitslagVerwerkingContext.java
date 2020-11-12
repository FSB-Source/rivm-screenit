package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.model.project.ProjectBestand;
import nl.topicuszorg.util.collections.CollectionUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import liquibase.util.csv.CSVReader;

public class ProjectUitslagVerwerkingContext
{
	private static final Logger LOG = LoggerFactory.getLogger(ProjectUitslagVerwerkingContext.class);

	private static final String BARCODE = "barcode";

	private static final String UITSLAG = "uitslag";

	private static final String ANALYSE_DATUM = "analysedatum";

	private static final String BRON = "bron";

	private ProjectBestand uitslagenBestand;

	private CSVReader reader = null;

	private int regelnummer;

	private String[] huidigeLine = null;

	private int barcodeColumn = -1;

	private int uitslagColumn = -1;

	private int analysedatumColumn = -1;

	private int bronColumn = -1;

	public ProjectUitslagVerwerkingContext(ProjectBestand uitslagenBestand, File file) throws Exception
	{
		this.uitslagenBestand = uitslagenBestand;
		getMostUsefullReader(file);
		bepaalVolgordeHeader();
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
					LOG.error("Van het uitslagbestand kan de reader niet worden gesloten", e);
					throw e;
				}
			}
		}
	}

	private void bepaalVolgordeHeader()
	{
		List<String> headers = getHuidigeRegel();
		if (CollectionUtils.isNotEmpty(headers))
		{
			for (String header : headers)
			{

				String geformatteerdeHeader = header.toLowerCase().trim();
				if (BARCODE.equals(geformatteerdeHeader))
				{
					barcodeColumn = headers.indexOf(header);
				}
				if (UITSLAG.equals(geformatteerdeHeader))
				{
					uitslagColumn = headers.indexOf(header);
				}
				if (ANALYSE_DATUM.equals(geformatteerdeHeader))
				{
					analysedatumColumn = headers.indexOf(header);
				}
				if (BRON.equals(geformatteerdeHeader))
				{
					bronColumn = headers.indexOf(header);
				}

			}
			if (barcodeColumn == -1 || uitslagColumn == -1 || analysedatumColumn == -1 || bronColumn == -1)
			{
				throw new IllegalStateException("Niet de juiste headers gevonden in het bestand (barcode, uitslag, analysedatum en bron).");
			}
		}
	}

	public List<String> getHuidigeRegel()
	{
		return Arrays.asList(huidigeLine);
	}

	public ProjectBestand getUitslagenBestand()
	{
		return uitslagenBestand;
	}

	public int getRegelnummer()
	{
		return regelnummer;
	}

	public String getBarcodeVanHuidigeRegel()
	{
		return huidigeLine[barcodeColumn];
	}

	public String getUitslagVanHuidigeRegel()
	{
		return huidigeLine[uitslagColumn];
	}

	public String getAnalyseDatum()
	{
		return huidigeLine[analysedatumColumn];
	}

	public String getBron()
	{
		return huidigeLine[bronColumn];
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

	public void close() throws IOException
	{
		if (reader != null)
		{
			reader.close();
		}
	}
}
