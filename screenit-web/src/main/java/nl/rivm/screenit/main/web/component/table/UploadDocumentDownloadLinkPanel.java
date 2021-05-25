
package nl.rivm.screenit.main.web.component.table;

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

import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;

import nl.rivm.screenit.main.web.component.AjaxDownload;
import nl.rivm.screenit.model.UploadDocument;
import nl.topicuszorg.documentupload.services.UploadDocumentService;

import org.apache.commons.io.IOUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.resource.AbstractResourceStreamWriter;
import org.apache.wicket.util.resource.IResourceStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class UploadDocumentDownloadLinkPanel extends GenericPanel<UploadDocument>
{
	private static final Logger LOG = LoggerFactory.getLogger(UploadDocumentDownloadLinkPanel.class);

	private static final long serialVersionUID = 1L;

	@SpringBean
	private UploadDocumentService uploadDocumentService;

	public UploadDocumentDownloadLinkPanel(String id, IModel<UploadDocument> model)
	{
		super(id, model);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		final AjaxDownload download = new AjaxDownload()
		{
			@Override
			protected IResourceStream getResourceStream()
			{
				AbstractResourceStreamWriter rstream = new AbstractResourceStreamWriter()
				{

					@Override
					public void write(OutputStream output) throws IOException
					{

						try
						{
							File file = uploadDocumentService.load(UploadDocumentDownloadLinkPanel.this.getModelObject());

							try (FileInputStream fis = new FileInputStream(file);)
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

				return rstream;
			}

			@Override
			protected String getFileName()
			{
				return getModelObject().getNaam();
			}
		};
		getPage().add(download);
		add(new IndicatingAjaxLink<UploadDocument>("download", getModel())
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				UploadDocumentDownloadLinkPanel.this.onBeforeDownloadClick(target);
				download.initiate(target);
			}
		});
	}

	protected void onBeforeDownloadClick(AjaxRequestTarget target)
	{

	}

	@Override
	protected void onConfigure()
	{
		super.onConfigure();
		UploadDocument document = getModelObject();
		setVisible(document != null && !Boolean.FALSE.equals(document.getActief()));
	}
}
