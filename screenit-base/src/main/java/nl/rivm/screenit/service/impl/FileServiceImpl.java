package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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
import java.io.IOException;
import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.service.FileService;
import nl.topicuszorg.documentupload.services.UploadDocumentService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class FileServiceImpl implements FileService
{

	private static final Logger LOG = LoggerFactory.getLogger(FileServiceImpl.class);

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Override
	public void delete(UploadDocument document)
	{
		uploadDocumentService.delete(document);
	}

	@Override
	public void delete(UploadDocument document, boolean deleteFile)
	{
		uploadDocumentService.delete(document, deleteFile);
	}

	@Override
	public File load(UploadDocument uploadDocument)
	{
		return uploadDocumentService.load(uploadDocument);
	}

	@Override
	public void saveOrUpdateUploadDocument(UploadDocument document, FileStoreLocation fileStoreLocation) throws IOException, IllegalStateException
	{
		saveOrUpdateUploadDocument(document, fileStoreLocation, null, true);
	}

	@Override
	public void saveOrUpdateUploadDocument(UploadDocument document, FileStoreLocation fileStoreLocation, Long id) throws IOException, IllegalStateException
	{
		saveOrUpdateUploadDocument(document, fileStoreLocation, id, true);
	}

	@Override
	public void saveOrUpdateUploadDocument(UploadDocument document, FileStoreLocation fileStoreLocation, Long id, boolean verwijderTmpFile) throws IOException,
		IllegalArgumentException
	{
		if (document == null)
		{
			throw new IllegalArgumentException("UploadDocument is leeg.");
		}

		if (id == null && fileStoreLocation.isSaveFileWithId())
		{
			throw new IllegalArgumentException("Id verwacht voor het opslaan van de juiste directory");
		}

		File tmpFile = null;
		if (verwijderTmpFile)
		{
			tmpFile = document.getFile();
		}

		String path = null;
		if (id != null)
		{
			path = fileStoreLocation.getPath(id);
		}
		else
		{
			path = fileStoreLocation.getPath();
		}

		uploadDocumentService.saveUploadDocument(document, null, path);
		hibernateService.saveOrUpdate(document);

		if (tmpFile != null)
		{
			tmpFile.delete();
		}
	}

	@Override
	public void updateUploadDocument(UploadDocument uploadDocument) throws IOException
	{
		uploadDocumentService.updateUploadDocument(uploadDocument);
	}

	@Override
	public void deleteDocumentFromList(UploadDocument document, List<UploadDocument> documents)
	{
		try
		{
			for (Iterator<UploadDocument> it = documents.iterator(); it.hasNext();)
			{
				UploadDocument uplDoc = it.next();
				if (uplDoc.getId().equals(document.getId()))
				{
					it.remove();

				}
			}
			delete(document, true);
		}
		catch (Exception e)
		{
			LOG.error("Er is een fout opgetreden! " + e.getMessage(), e);
		}
	}
}
