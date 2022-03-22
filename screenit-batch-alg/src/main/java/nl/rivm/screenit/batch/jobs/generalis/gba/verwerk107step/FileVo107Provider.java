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
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.gba.GbaVerwerkingsLog;

import org.apache.commons.io.IOUtils;

@Slf4j
public class FileVo107Provider implements IVo107Provider
{
	@Setter
	private String vo107File;

	@Setter
	private boolean fromClasspath;

	@Override
	public List<Vo107File> getVo107Files(GbaVerwerkingsLog verwerkingsLog)
	{
		String path = vo107File;

		if (fromClasspath)
		{
			path = FileVo107Provider.class.getResource(vo107File).getFile();
		}

		List<Vo107File> files = new ArrayList<>();
		File file = new File(path);
		if (file.isDirectory())
		{
			for (File subFile : file.listFiles())
			{
				files.add(new Vo107ClasspathFile(subFile.getAbsolutePath()));
			}
		}
		else
		{
			files.add(new Vo107ClasspathFile(path));
		}

		return files;
	}

	private static class Vo107ClasspathFile implements Vo107File
	{
		private final String path;

		public Vo107ClasspathFile(String path)
		{
			this.path = path;
		}

		@Override
		public void saveToFile(File targetFile)
		{
			FileInputStream fileInputStream = null;
			FileOutputStream fileOutputStream = null;
			try
			{
				fileInputStream = new FileInputStream(path);
				fileOutputStream = new FileOutputStream(targetFile);
				IOUtils.copyLarge(fileInputStream, fileOutputStream);
			}
			catch (FileNotFoundException e)
			{
				LOG.error("File not found", e);
			}
			catch (IOException e)
			{
				LOG.error("Error copying file", e);
			}
			finally
			{
				try
				{
					if (fileInputStream != null)
					{
						fileInputStream.close();
					}

					if (fileOutputStream != null)
					{
						fileOutputStream.close();
					}
				}
				catch (IOException e)
				{
					LOG.error("Error closing stream", e);
				}
			}
		}

		@Override
		public String getFilename()
		{
			return path;
		}

		@Override
		public void deleteFile()
		{

		}
	}
}
