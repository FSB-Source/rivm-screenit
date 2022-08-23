package nl.rivm.screenit.batch.jobs.generalis.huisarts.importstep;

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

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.CsvFileProvider;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import au.com.bytecode.opencsv.CSVReader;

@Slf4j
@Component
public class EnovationHuisartsCsvFileProvider implements CsvFileProvider
{
	@Autowired
	private SimplePreferenceService preferenceService;

	@Override
	public Iterator<CSVReader> getReaders() throws IllegalStateException, IOException
	{
		List<CSVReader> readers = new ArrayList<>();

		String enovationUrl = preferenceService.getString(PreferenceKey.INTERNAL_ZORGMAIL_BESTAND_URL.name());

		if (StringUtils.isNotBlank(enovationUrl))
		{
			URL url = new URL(enovationUrl);
			URLConnection uc = url.openConnection();

			if (url.getUserInfo() != null)
			{
				String basicAuth = "Basic " + javax.xml.bind.DatatypeConverter.printBase64Binary(url.getUserInfo().getBytes());
				uc.setRequestProperty("Authorization", basicAuth);
			}

			InputStream in = uc.getInputStream();

			ZipInputStream zipInputStream = new ZipInputStream(in);
			ZipEntry nextEntry = zipInputStream.getNextEntry();
			String sourceFileNaam = nextEntry.getName() + " (" + enovationUrl + ")";
			CSVFileReader reader = new CSVFileReader(new InputStreamReader(zipInputStream), ',', sourceFileNaam);

			readers.add(reader);

			LOG.info("Reader toegevoegd voor file van url: " + enovationUrl);
		}
		else
		{
			LOG.error("Geen URL voor zorgmailadresboek ingevoerd in technisch beheer. (INTERNAL_ZORGMAIL_BESTAND_URL)");
		}

		return new CSVReaderIterator(readers.iterator());
	}

}
