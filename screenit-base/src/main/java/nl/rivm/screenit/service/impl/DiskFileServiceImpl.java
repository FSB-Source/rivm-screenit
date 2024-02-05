package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.service.FileService;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@ConditionalOnProperty(value = "s3.enabled", havingValue = "false")
public class DiskFileServiceImpl implements FileService
{
	public DiskFileServiceImpl()
	{
		LOG.info("diskfile service");
	}

	@Override
	public boolean exists(String fullFilePath)
	{
		if (StringUtils.isBlank(fullFilePath))
		{
			throw new IllegalStateException("Er is geen pad opgegeven om te controleren");
		}

		return new File(fullFilePath).exists();
	}

	@Override
	public void save(String fullFilePath, File tempFile) throws IOException
	{
		try (var fileInputStream = new FileInputStream(tempFile))
		{
			save(fullFilePath, fileInputStream, tempFile.length());
		}
	}

	@Override
	public void save(String fullFilePath, InputStream content, Long contentLength) throws IOException
	{
		if (StringUtils.isBlank(fullFilePath))
		{
			throw new IllegalStateException("Er is geen pad opgegeven om op te slaan");
		}

		try
		{
			File file = new File(fullFilePath);
			file.getParentFile().mkdirs();
			if (!file.createNewFile())
			{
				LOG.warn("Bestand {} bestond al, maar wordt overschreven", fullFilePath);
			}

			if (content != null)
			{
				try (var outputStream = new FileOutputStream(file))
				{
					IOUtils.copy(content, outputStream);
				}
			}
			else if (contentLength > 0)
			{
				throw new IllegalStateException("Poging om bestand " + fullFilePath + " om te slaan zonder content, maar verwachtte " + contentLength + " bytes");
			}
		}
		catch (IOException e)
		{
			LOG.error("Fout bij opslaan van het bestand {} door {}", fullFilePath, e.getMessage());
			throw e;
		}
	}

	@Override
	public File load(String fullFilePath)
	{
		if (StringUtils.isBlank(fullFilePath))
		{
			throw new IllegalStateException("Er is geen pad opgegeven om in te laden");
		}

		return new File(fullFilePath);
	}

	@Override
	public InputStream loadAsStream(String fullFilePath) throws IOException
	{
		if (StringUtils.isBlank(fullFilePath))
		{
			throw new IllegalStateException("Er is geen pad opgegeven om in te laden");
		}

		try
		{
			return Files.newInputStream(Paths.get(fullFilePath));
		}
		catch (IOException e)
		{
			LOG.error("Kon bestand {} niet inladen als stream door {}", fullFilePath, e.getMessage());
			throw e;
		}
	}

	@Override
	public boolean delete(String fullFilePath)
	{
		if (StringUtils.isBlank(fullFilePath))
		{
			throw new IllegalStateException("Er is geen pad opgegeven om te verwijderen");
		}
		else if (!exists(fullFilePath))
		{
			return false;
		}
		return new File(fullFilePath).delete();
	}

	@Override
	public boolean deleteQuietly(String fullFilePath)
	{
		return FileUtils.deleteQuietly(new File(fullFilePath));
	}

	@Override
	public void cleanDirectory(String directory) throws IOException
	{
		try
		{
			var file = new File(directory);

			if (file.exists())
			{
				FileUtils.cleanDirectory(file);
			}
		}
		catch (IOException e)
		{
			LOG.error("Kon map {} niet opschonen door {}", directory, e.getMessage());
			throw e;
		}
	}

	@Override
	public void deleteDirectory(String directory) throws IOException
	{
		try
		{
			FileUtils.deleteDirectory(new File(directory));
		}
		catch (IOException e)
		{
			LOG.error("Kon map {} niet verwijderen door {}", directory, e.getMessage());
			throw e;
		}
	}

	@Override
	public void deleteFileOrDirectory(String bestand) throws IOException
	{
		var bestandVanFileStore = load(bestand);

		if (bestandVanFileStore.isDirectory())
		{
			deleteDirectory(bestandVanFileStore.getPath());
		}
		else
		{
			delete(bestandVanFileStore.getPath());
		}
	}

	@Override
	public List<String> listFiles(String directory)
	{
		return Arrays.stream(Objects.requireNonNull(new File(directory).listFiles())).map(File::getPath).collect(Collectors.toList());
	}
}
