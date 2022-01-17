package nl.rivm.screenit.batch.jobs.mamma.palga.csvimport;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.batch.CsvFileProvider;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.mamma.MammaPalgaService;

import org.springframework.beans.factory.annotation.Autowired;

import au.com.bytecode.opencsv.CSVReader;

public class MammaPalgaCsvImportProvider implements CsvFileProvider
{
	@Autowired
	private String locatieFilestore;

	@Autowired
	private MammaPalgaService palgaService;

	@Autowired
	private FileService fileService;

	@Override
	public Iterator<CSVReader> getReaders() throws FileNotFoundException
	{
		List<CSVReader> readers = new ArrayList<>();
		readers.add(new CSVReader(new FileReader(fileService.load(palgaService.getImport())), ','));
		return new CSVReaderIterator(readers.iterator());
	}
}
