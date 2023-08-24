package nl.rivm.screenit.main.web.component.table;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.web.component.AjaxDownload;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.io.IOUtils;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.resource.AbstractResourceStreamWriter;
import org.apache.wicket.util.resource.IResourceStream;

@Slf4j
public class UploadDocumentDownloadBehavior extends AjaxDownload
{
	private final IModel<String> fileNameToLog;

	private final IModel<UploadDocument> uploadDocumentModel;

	@Getter
	private final Long uploadDocumentId;

	@SpringBean
	private UploadDocumentService uploadDocumentService;

	public UploadDocumentDownloadBehavior(IModel<String> fileNameToLog, IModel<UploadDocument> uploadDocumentModel)
	{
		super();
		this.fileNameToLog = fileNameToLog;
		this.uploadDocumentModel = uploadDocumentModel;
		uploadDocumentId = uploadDocumentModel.getObject().getId();
	}

	@Override
	protected IResourceStream getResourceStream()
	{
		return new AbstractResourceStreamWriter()
		{

			@Override
			public void write(OutputStream output)
			{

				try
				{
					File file = uploadDocumentService.load(uploadDocumentModel.getObject());

					try (FileInputStream fis = new FileInputStream(file))
					{
						IOUtils.copy(fis, output);
					}
				}
				catch (IOException e)
				{
					LOG.error("Fout bij het opleveren van upload document: " + e.getMessage(), e);
				}
				finally
				{
					close(output);
				}
			}

			private void close(Closeable closable)
			{
				if (closable != null)
				{
					try
					{
						closable.close();
					}
					catch (IOException e)
					{
						LOG.error("Fout bij het sluiten van stream: " + e.getMessage(), e);
					}
				}
			}
		};
	}

	@Override
	protected String getFileName()
	{
		return uploadDocumentModel.getObject().getNaam();
	}

	@Override
	protected String getFileNameToLog()
	{
		return ModelUtil.nullSafeGet(fileNameToLog);
	}
}
