
package nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.cdaverslag;

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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import nl.rivm.screenit.model.logging.BerichtOntvangenLogEvent;

import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.ResourceLink;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.request.resource.AbstractResource;
import org.apache.wicket.request.resource.ContentDisposition;

public class CdaVerslagErrorDownloadCdaPanel extends GenericPanel<BerichtOntvangenLogEvent>
{

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(CdaVerslagErrorDownloadCdaPanel.class);

	public CdaVerslagErrorDownloadCdaPanel(String id, IModel<BerichtOntvangenLogEvent> model)
	{
		super(id, new CompoundPropertyModel<>(model));

		add(DateLabel.forDatePattern("logRegel.gebeurtenisDatum", "dd-MM-yyyy HH:mm:ss"));
		add(new Label("melding"));
		add(new ResourceLink<>("download", new AbstractResource()
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected ResourceResponse newResourceResponse(Attributes attributes)
			{
				ResourceResponse response = new ResourceResponse();
				response.setFileName("bericht.xml");
				response.setContentType("application/xml");
				response.getHeaders().addHeader("Cache-Control", "no-cache");
				response.setContentDisposition(ContentDisposition.ATTACHMENT);

				response.setWriteCallback(new WriteCallback()
				{

					@Override
					public void writeData(Attributes attributes)
					{
						try
						{
							String xmlBericht = CdaVerslagErrorDownloadCdaPanel.this.getModelObject().getBericht().getXmlBericht();
							try (InputStream writer = IOUtils.toInputStream(xmlBericht); OutputStream outputStream = attributes.getResponse().getOutputStream();)
							{
								IOUtils.copy(writer, outputStream);
							}
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
				}

				);
				return response;
			}
		}

		)).

		setVisible(getModelObject()

		.

		getBericht()

		!= null);
	}
}
