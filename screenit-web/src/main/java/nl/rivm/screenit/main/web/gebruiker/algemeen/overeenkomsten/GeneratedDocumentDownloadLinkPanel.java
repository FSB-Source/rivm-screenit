
package nl.rivm.screenit.main.web.gebruiker.algemeen.overeenkomsten;

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

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;

import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.overeenkomsten.AbstractAfgeslotenOvereenkomst;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.FileService;

import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.apache.wicket.markup.html.link.ResourceLink;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.request.resource.AbstractResource;
import org.apache.wicket.request.resource.ContentDisposition;
import org.apache.wicket.spring.injection.annot.SpringBean;

import com.aspose.words.Document;
import com.aspose.words.OoxmlSaveOptions;
import com.aspose.words.SaveFormat;

public class GeneratedDocumentDownloadLinkPanel extends Panel
{

	private static final Logger LOG = LoggerFactory.getLogger(GeneratedDocumentDownloadLinkPanel.class);

	@SpringBean
	private AsposeService asposeService;

	@SpringBean
	private FileService fileService;

	private static final long serialVersionUID = 1L;

	public GeneratedDocumentDownloadLinkPanel(String id, final IModel<AbstractAfgeslotenOvereenkomst> model)
	{
		super(id, model);
		add(new ResourceLink<>("download", new AbstractResource()
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected ResourceResponse newResourceResponse(Attributes attributes)
			{
				ResourceResponse response = new ResourceResponse();
				response.setFileName(model.getObject().getCode().replace(" ", "_") + ".docx");
				response.setContentType("application/vnd.openxmlformats-officedocument.wordprocessingml.document");
				response.getHeaders().addHeader("Cache-Control", "no-cache");
				response.setContentDisposition(ContentDisposition.ATTACHMENT);

				response.setWriteCallback(new WriteCallback()
				{

					@Override
					public void writeData(Attributes attributes)
					{
						try (OutputStream outputStream = attributes.getResponse().getOutputStream();)
						{
							File file = fileService.load(model.getObject().getOvereenkomst().getDocument());
							MailMergeContext mailMergeContext = new MailMergeContext();
							mailMergeContext.setOvereenkomst(model.getObject());
							Document document = asposeService.processDocument(FileUtils.readFileToByteArray(file), mailMergeContext);

							document.save(outputStream, new OoxmlSaveOptions(SaveFormat.DOCX));
						}
						catch (IOException e)
						{
							LOG.error("Fout bij laden uploadcoument: " + e.getMessage(), e);
						}
						catch (Exception e)
						{
							LOG.error("Error bij mailmerge: " + e.getMessage(), e);
						}
					}
				});
				return response;
			}
		}));
	}
}
