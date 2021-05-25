
package nl.rivm.screenit.batch;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.io.Closeable;
import java.io.IOException;
import java.io.Reader;
import java.util.Iterator;

import au.com.bytecode.opencsv.CSVReader;

public interface CsvFileProvider
{

	Iterator<CSVReader> getReaders() throws Exception;

	class CSVFileReader extends CSVReader
	{

		private final String sourceFileNaam;

		public CSVFileReader(Reader reader, char separator, String sourceFileNaam)
		{
			super(reader, separator);
			this.sourceFileNaam = sourceFileNaam;
		}

		String getFileNaam()
		{
			return sourceFileNaam;
		}

	}

	class CSVReaderIterator implements Iterator<CSVReader>, Closeable
	{

		private final Iterator<CSVReader> readers;

		private CSVReader current;

		public CSVReaderIterator(Iterator<CSVReader> readers)
		{
			this.readers = readers;
		}

		@Override
		public boolean hasNext()
		{
			return readers.hasNext();
		}

		@Override
		public CSVReader next()
		{
			if (current != null)
			{
				try
				{
					current.close();
				}
				catch (IOException e)
				{
					throw new IllegalStateException(e);
				}
			}
			if (readers.hasNext())
			{
				current = readers.next();

				return current;
			}
			return null;
		}

		@Override
		public void remove()
		{
			readers.remove();
		}

		@Override
		public void close() throws IOException
		{
			if (current != null)
			{
				current.close();
			}
			while (readers.hasNext())
			{
				current = readers.next();
				current.close();
			}

		}
	}
}
