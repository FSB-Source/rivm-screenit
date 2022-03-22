package nl.rivm.screenit.main.web.component.table;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.web.component.AjaxDownload;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.io.IOUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.resource.AbstractResourceStreamWriter;
import org.apache.wicket.util.resource.IResourceStream;

@Slf4j
public class UploadDocumentDownloadLinkPanel extends GenericPanel<UploadDocument>
{
	private final IModel<String> fileNameToLog;

	@SpringBean
	private UploadDocumentService uploadDocumentService;

	public UploadDocumentDownloadLinkPanel(String id, IModel<UploadDocument> model)
	{
		this(id, model, null);
	}

	public UploadDocumentDownloadLinkPanel(String id, IModel<UploadDocument> model, IModel<String> fileNameToLog)
	{
		super(id, model);
		this.fileNameToLog = fileNameToLog;
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
				return new AbstractResourceStreamWriter()
				{

					@Override
					public void write(OutputStream output)
					{

						try
						{
							File file = uploadDocumentService.load(UploadDocumentDownloadLinkPanel.this.getModelObject());

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
				return getModelObject().getNaam();
			}

			@Override
			protected String getFileNameToLog()
			{
				return ModelUtil.nullSafeGet(fileNameToLog);
			}

		};
		getPage().add(download);
		add(new IndicatingAjaxLink<>("download", getModel())
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
