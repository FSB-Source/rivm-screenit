package nl.rivm.screenit.main.web.component.validator;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.model.enums.FileType;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.validation.IValidatable;
import org.apache.wicket.validation.IValidator;
import org.apache.wicket.validation.ValidationError;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class FileValidator implements IValidator<List<FileUpload>>
{
	private final FileType[] fileTypes;

	private final Logger LOG = LoggerFactory.getLogger(FileValidator.class);

	public FileValidator(FileType... fileTypes)
	{
		this.fileTypes = fileTypes;
	}

	@Override
	public void validate(IValidatable<List<FileUpload>> validatable)
	{
		var files = validatable.getValue();
		var allFileExtensies = new ArrayList<String>();
		var naamFileTypes = new ArrayList<String>();

		for (var fileType : fileTypes)
		{
			allFileExtensies.addAll(fileType.getFileExtensies());
			naamFileTypes.add(fileType.getNaamFileType());
		}

		for (FileUpload file : files)
		{
			FileType fileTypeMatch = null;
			var fileName = file.getClientFileName();
			for (var fileType : fileTypes)
			{
				if (fileType.getAllowedContentTypes().contains(file.getContentType())
					&& fileType.getFileExtensies().stream().anyMatch(fileName::endsWith))
				{
					byte[] magicNumber = fileType.getMagicNumber();
					if (magicNumber != null)
					{
						var offset = fileType.getOffset();
						if (file.getBytes().length >= offset + magicNumber.length)
						{
							byte[] magicNumberUpload = Arrays.copyOfRange(file.getBytes(), offset, offset + magicNumber.length);
							if (Arrays.equals(magicNumberUpload, magicNumber))
							{
								fileTypeMatch = fileType;
								break;
							}
						}
					}
					else
					{
						fileTypeMatch = fileType;
						break;
					}
				}
			}

			if (fileTypeMatch == null)
			{
				var error = new ValidationError();
				error.setVariable("filename", fileName);
				LOG.error("Contenttype is niet valide! (ContentType: {}, Toegestane filetypes: {})", file.getContentType(),
					StringUtils.join(naamFileTypes, " of "));
				if (allFileExtensies.stream().noneMatch(fileName::endsWith))
				{
					error.addKey("FileValidator.extensie");
					error.setVariable("extensies", StringUtils.join(allFileExtensies, " of "));
				}
				else
				{
					error.addKey("FileValidator");
					error.setVariable("fileTypes", StringUtils.join(naamFileTypes, " of "));
				}
				validatable.error(error);
			}
		}
	}
}
