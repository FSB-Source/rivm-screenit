package nl.rivm.screenit.main.service.impl;

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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.topicuszorg.util.collections.CollectionUtils;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import au.com.bytecode.opencsv.CSVReader;

public class KoppelresultatenKankerregistratieVerwerkingContext implements AutoCloseable
{

	private static final Logger LOG = LoggerFactory.getLogger(KoppelresultatenKankerregistratieVerwerkingContext.class);

	private CSVReader reader = null;

	private String[] huidigeLine = null;

	public interface AbstractKoppelresultatenKankerregistratieHeaderMapping
	{
		String getHeader();

		String getFieldName();
	}

	public enum ColonKoppelresultatenKankerregistratieHeaderMapping implements AbstractKoppelresultatenKankerregistratieHeaderMapping
	{
		CLIENTNUMMER("ClientnrBVO", null),

		GEBOORTEDATUM("Geboortedatum", null),

		UITNODIGINGSNUMMER("UitnodigingsnrBVO", null),

		SCREENINGSDATUM("Screeningsdatum", null),

		BVO("BVO", null),

		INCIDENTIEDATUM("Incidentiedatum", "incidentiedatum"),

		EID("TumorNr", "eid"),

		CODEZIEKENHUISVANDIAGNOSE("ZhsCode", "codeZiekenhuisVanDiagnose"),

		OMSCHRIJVINGZIEKENHUISVANDIAGNOSE("ZhsNaam", "omschrijvingZiekenhuisVanDiagnose"),

		REDENDIAGNOSE("RedenDiagnose", "redenDiagnose"),

		TOPOGRAFIE("Topografie", "topografie"),

		MORFOLOGIE("Morfologie", "morfologie"),

		TUMORGEDRAG("Tumorgedrag", "tumorgedrag"),

		DIFFERENTIATIEGRAAD("Differentiatiegraad", "differentiatiegraad"),

		CTNM("cTNM", "cTNM"),

		PTNM("pTNM", "pTNM"),

		YPTNM("ypTNM", "ypTNM");

		private final String header;

		private final String fieldName;

		ColonKoppelresultatenKankerregistratieHeaderMapping(String header, String fieldName)
		{
			this.header = header;
			this.fieldName = fieldName;
		}

		@Override
		public String getHeader()
		{
			return header;
		}

		@Override
		public String getFieldName()
		{
			return fieldName;
		}
	}

	private int regelnummer;

	private Map<AbstractKoppelresultatenKankerregistratieHeaderMapping, Integer> mapping = new HashMap<>();

	private final Bevolkingsonderzoek bevolkingsonderzoek;

	private List<String> meldingen = new ArrayList<>();

	private UploadDocument document;

	public KoppelresultatenKankerregistratieVerwerkingContext(File file, String contentType, String fileName, Bevolkingsonderzoek bevolkingsonderzoek) throws Exception
	{
		this.bevolkingsonderzoek = bevolkingsonderzoek;
		getMostUsefullReader(file);
		bepaalVolgordeHeaders();
		regelnummer = 1;
		document = new UploadDocument();
		document.setActief(Boolean.TRUE);
		document.setContentType(contentType);
		document.setFile(file);
		document.setNaam(fileName);
	}

	public UploadDocument getFile()
	{
		return document;
	}

	private void getMostUsefullReader(File file) throws Exception
	{
		CSVReader testReader = null;
		try
		{
			testReader = new CSVReader(new FileReader(file), ',');
			huidigeLine = testReader.readNext();
			if (huidigeLine != null && Arrays.asList(huidigeLine).size() >= 17)
			{
				reader = testReader;
			}
			else
			{
				testReader.close();
				huidigeLine = null;
				testReader = new CSVReader(new FileReader(file), ';');
				huidigeLine = testReader.readNext();
				if (huidigeLine != null && Arrays.asList(huidigeLine).size() >= 17)
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
					LOG.error("Van het bestand kan de reader niet worden gesloten", e);
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
			int i = 0;
			for (String header : headers)
			{
				boolean gevonden = false;
				for (ColonKoppelresultatenKankerregistratieHeaderMapping m : ColonKoppelresultatenKankerregistratieHeaderMapping.values())
				{
					if (m.header.equals(header))
					{
						mapping.put(m, i);
						gevonden = true;
						break;
					}
				}
				if (!gevonden)
				{
					throw new IllegalStateException("Header " + header + " niet bekend.");
				}
				i++;
			}
		}
	}

	public String getColumnValue(AbstractKoppelresultatenKankerregistratieHeaderMapping key)
	{
		return StringUtils.trim(getHuidigeRegel().get(mapping.get(key)));
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

	@Override
	public void close() throws IOException
	{
		if (reader != null)
		{
			reader.close();
		}
	}

	public Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return bevolkingsonderzoek;
	}

	public void addMelding(String melding)
	{
		meldingen.add(melding);
	}

	public List<String> getMeldingen()
	{
		return meldingen;
	}
}
