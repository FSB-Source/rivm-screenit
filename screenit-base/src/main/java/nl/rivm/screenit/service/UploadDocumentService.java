package nl.rivm.screenit.service;

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
import java.io.IOException;
import java.util.List;

import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.FileStoreLocation;

public interface UploadDocumentService
{
	void delete(UploadDocument document);

	File load(UploadDocument uploadDocument);

	void saveOrUpdate(UploadDocument document, FileStoreLocation fileStoreLocation) throws IOException, IllegalStateException;

	void saveOrUpdate(UploadDocument document, FileStoreLocation fileStoreLocation, Long id) throws IOException, IllegalStateException;

	void saveOrUpdate(UploadDocument document, FileStoreLocation fileStoreLocation, Long id, boolean verwijderTmpFile) throws IOException, IllegalStateException;

	void update(UploadDocument uploadDocument) throws IOException;

	void deleteDocumentFromList(UploadDocument document, List<UploadDocument> documents);
}
