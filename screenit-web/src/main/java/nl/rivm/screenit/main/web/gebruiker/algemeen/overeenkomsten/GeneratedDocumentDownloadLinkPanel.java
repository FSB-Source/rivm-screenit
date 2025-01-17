package nl.rivm.screenit.main.web.gebruiker.algemeen.overeenkomsten;

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

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.overeenkomsten.AbstractAfgeslotenOvereenkomst;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.UploadDocumentService;

import org.apache.commons.io.FileUtils;
import org.apache.wicket.markup.html.link.ResourceLink;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.request.resource.AbstractResource;
import org.apache.wicket.request.resource.ContentDisposition;
import org.apache.wicket.spring.injection.annot.SpringBean;

import com.aspose.words.Document;
import com.aspose.words.OoxmlSaveOptions;
import com.aspose.words.SaveFormat;

@Slf4j
public class GeneratedDocumentDownloadLinkPanel extends Panel
{
	@SpringBean
	private AsposeService asposeService;

	@SpringBean
	private UploadDocumentService uploadDocumentService;

	public GeneratedDocumentDownloadLinkPanel(String id, final IModel<AbstractAfgeslotenOvereenkomst> model)
	{
		super(id, model);
		add(new ResourceLink<>("download", new AbstractResource()
		{
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
							File file = uploadDocumentService.load(model.getObject().getOvereenkomst().getDocument());
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
