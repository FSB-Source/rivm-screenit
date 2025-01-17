package nl.rivm.screenit.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.io.InputStream;
import java.util.List;

public interface FileService
{

	boolean exists(String fullFilePath);

	void save(String fullFilePath, File tempFile) throws IOException;

	void save(String fullFilePath, InputStream content, Long contentLength) throws IOException;

	File load(String fullFilePath);

	InputStream loadAsStream(String fullFilePath) throws IOException;

	boolean delete(String fullFilePath);

	boolean deleteQuietly(String fullFilePath);

	void cleanDirectory(String directory) throws IOException;

	void deleteDirectory(String directory) throws IOException;

	void deleteFileOrDirectory(String bestand) throws IOException;

	List<String> listFiles(String directory) throws IOException;

	List<String> listFilesGesorteerd(String directory) throws IOException;

}
