package nl.rivm.screenit.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import lombok.extern.slf4j.Slf4j;

import net.lingala.zip4j.ZipFile;
import net.lingala.zip4j.exception.ZipException;
import net.lingala.zip4j.model.FileHeader;

import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.topicuszorg.spring.injection.SpringBeanProvider;

import org.apache.commons.lang3.StringUtils;

@Slf4j
public final class ZipUtil
{
	public static Set<File> maakZips(List<UploadDocument> uploadDocumenten, String baseZipNaam, long maxKiloBytesZip) throws IOException
	{
		UploadDocumentService uploadDocumentService = SpringBeanProvider.getInstance().getBean(UploadDocumentService.class);
		Set<File> zips = new LinkedHashSet<>();
		int zipNummer = 0;
		File zipFile = null;
		ZipOutputStream zipOut = null;
		int aantalBestandenInZip = 0;

		for (UploadDocument document : uploadDocumenten)
		{
			File fileToZip = uploadDocumentService.load(document);
			long zipKiloBytes = zipFile == null ? 0 : (zipFile.length() + fileToZip.length()) / 1024;

			if (zipNummer == 0 || zipKiloBytes >= (maxKiloBytesZip))
			{
				if (zipNummer != 0)
				{
					LOG.info("Aantal bestanden in zip" + zipNummer + ": " + aantalBestandenInZip);
					aantalBestandenInZip = 0;
				}
				String zipNaam = maakZipNaam(baseZipNaam, ++zipNummer);
				zipFile = File.createTempFile(zipNaam, ".zip");
				zips.add(zipFile);
				zipOut = closeEnMaakZipOut(zipFile, zipOut);
			}
			aantalBestandenInZip++;
			addEntryToZip(document.getNaam(), fileToZip.getPath(), zipOut);
			zipOut.flush();
		}
		LOG.info("Aantal bestanden in zip" + zipNummer + ": " + aantalBestandenInZip);
		Objects.requireNonNull(zipOut).close();

		return zips;
	}

	private static ZipOutputStream closeEnMaakZipOut(File zipFile, ZipOutputStream zipOut) throws IOException
	{
		if (zipOut != null)
		{
			zipOut.close();
		}
		return new ZipOutputStream(new FileOutputStream(zipFile));
	}

	public static void zipFileOrDirectory(String fileToZip, String zipFile, boolean excludeContainingFolder) throws IOException
	{
		File srcFile = new File(fileToZip);
		String[] filesToZip = srcFile.list();
		try (ZipOutputStream zipOut = new ZipOutputStream(new FileOutputStream(zipFile));)
		{
			if (excludeContainingFolder && srcFile.isDirectory())
			{
				for (String fileName : filesToZip)
				{
					addToZip("", fileToZip + "/" + fileName, zipOut);
				}
			}
			else
			{
				addToZip("", fileToZip, zipOut);
			}

			zipOut.flush();
		}

		LOG.info("Successfully created " + zipFile);
	}

	private static void addToZip(String path, String srcFile, ZipOutputStream zipOut) throws IOException
	{
		File file = new File(srcFile);
		String filePath = "".equals(path) ? file.getName() : path + "/" + file.getName();
		if (file.isDirectory())
		{
			for (String fileName : file.list())
			{
				addToZip(filePath, srcFile + "/" + fileName, zipOut);
			}
		}
		else
		{
			addEntryToZip(filePath, srcFile, zipOut);
		}
	}

	private static void addEntryToZip(String entryPath, String srcFile, ZipOutputStream zipOut) throws IOException
	{
		zipOut.putNextEntry(new ZipEntry(entryPath));
		try (FileInputStream in = new FileInputStream(srcFile);)
		{

			byte[] buffer = new byte[1024];
			int len;
			while ((len = in.read(buffer)) != -1)
			{
				zipOut.write(buffer, 0, len);
			}
		}
	}

	private static String maakZipNaam(String baseZipNaam, int zipNummer)
	{
		return StringUtils.join(Arrays.asList(baseZipNaam, "zipNummer", zipNummer, "-"), "-");
	}

	public static List<File> extractZip(File file, String wachtwoord, String path, boolean verwijderZip) throws ZipException
	{
		ZipFile zipFile = new ZipFile(file);
		if (zipFile.isEncrypted())
		{
			if (wachtwoord != null)
			{
				zipFile.setPassword(wachtwoord.toCharArray());
			}
			else
			{
				throw new IllegalArgumentException("zipFile is encrypted, wachtwoord mag niet null zijn");
			}
		}

		List<File> files = new ArrayList<>();
		zipFile.extractAll(path);
		for (FileHeader fileHeader : zipFile.getFileHeaders())
		{
			String fileName = fileHeader.getFileName();
			files.add(new File(path + fileName));
		}

		if (verwijderZip)
		{
			file.delete();
		}
		return files;
	}

}
