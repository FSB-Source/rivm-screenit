package nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;

import nl.rivm.screenit.main.util.TiffUtil;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.wicket.model.IModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.apache.wicket.request.resource.AbstractResource;
import org.apache.wicket.request.resource.ContentDisposition;
import org.apache.wicket.util.time.Duration;

public class ScannedFormulierViewerResource extends AbstractResource
{

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(ScannedFormulierViewerResourceExternal.class);

	private final boolean alsAttachement;

	private Duration cacheDuration;

	private final IModel<File> fileModel;

	public ScannedFormulierViewerResource(IModel<File> fileModel, boolean alsAttachement)
	{
		this(fileModel, alsAttachement, null);
	}

	public ScannedFormulierViewerResource(IModel<File> fileModel, boolean alsAttachement, Duration cacheDuration)
	{
		this.fileModel = fileModel;
		this.alsAttachement = alsAttachement;
		this.cacheDuration = cacheDuration;
	}

	@Override
	protected ResourceResponse newResourceResponse(Attributes attributes)
	{
		ResourceResponse response = new ResourceResponse();

		if (alsAttachement)
		{
			response.setContentDisposition(ContentDisposition.ATTACHMENT);
		}

		response.setFileName("image.pdf");

		if (cacheDuration != null)
		{
			response.setCacheDuration(cacheDuration);
		}

		response.setWriteCallback(new WriteCallback()
		{

			@Override
			public void writeData(Attributes attributes)
			{
				try
				{
					ByteArrayOutputStream out = new ByteArrayOutputStream();
					PDDocument pdDocument = TiffUtil.tiffToPdfDocument(fileModel.getObject());
					pdDocument.save(out);
					pdDocument.close();

					try (InputStream inputStream = new ByteArrayInputStream(out.toByteArray()))
					{
						writeStream(attributes, inputStream);
						out.close();
					}
				}
				catch (Exception e)
				{
					e.printStackTrace();
				}
			}
		});

		return response;
	}
}
