package nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.io.IOException;
import java.util.Date;

import nl.topicuszorg.wicket.component.object.PdfObjectContainer;

import org.apache.commons.io.FileUtils;
import org.apache.wicket.IRequestListener;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.request.resource.AbstractResource;
import org.apache.wicket.request.resource.IResource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PdfViewer extends PdfObjectContainer implements IRequestListener
{

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(PdfViewer.class);

	private File tempFile;

	private boolean deleteFile;

	public PdfViewer(String id, File tempFile)
	{
		this(id, tempFile, false);
	}

	public PdfViewer(String id, File tempFile, boolean deleteFile)
	{
		super(id);
		this.tempFile = tempFile;
		this.deleteFile = deleteFile;
	}

	@Override
	protected void onInitialize()
	{

		setValue(DATA_ATTRIBUTE, urlForListener(null).toString() + "&random=" + new Date().getTime());
		super.onInitialize();
	}

	@Override
	public void onRequest()
	{
		IResource resource = new AbstractResource()
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected ResourceResponse newResourceResponse(Attributes attributes)
			{

				ResourceResponse response = new ResourceResponse();
				response.setFileName("test_document_templates.pdf");
				response.setWriteCallback(new WriteCallback()
				{
					@Override
					public void writeData(Attributes attributes)
					{
						try (FileInputStream pdfStream = new FileInputStream(tempFile))
						{
							writeStream(attributes, pdfStream);
						}
						catch (IOException e)
						{
							LOG.error("Fout bij laden uploadcoument: " + e.getMessage(), e);
						}
						finally
						{
							if (deleteFile)
							{
								FileUtils.deleteQuietly(tempFile);
							}
						}
					}

				});

				return response;
			}

		};
		IResource.Attributes a = new IResource.Attributes(RequestCycle.get().getRequest(), RequestCycle.get().getResponse(), null);
		resource.respond(a);
	}

	@Override
	public boolean rendersPage()
	{
		return false;
	}

}
