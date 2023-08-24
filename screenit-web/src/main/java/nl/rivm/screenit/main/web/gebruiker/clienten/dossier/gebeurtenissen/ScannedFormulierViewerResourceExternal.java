package nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen;

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
import java.net.URL;
import java.net.URLConnection;
import java.time.Duration;

import lombok.extern.slf4j.Slf4j;

import org.apache.wicket.request.resource.AbstractResource;
import org.apache.wicket.request.resource.ContentDisposition;

@Slf4j
public class ScannedFormulierViewerResourceExternal extends AbstractResource
{
	private final boolean alsAttachement;

	private Duration cacheDuration;

	private final String spec;

	public ScannedFormulierViewerResourceExternal(String spec, boolean alsAttachement)
	{
		this(spec, alsAttachement, null);
	}

	public ScannedFormulierViewerResourceExternal(String spec, boolean alsAttachement, Duration cacheDuration)
	{
		this.spec = spec;
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
					URLConnection c = getUrlConnection();
					try (InputStream inputStream = c.getInputStream())
					{
						writeStream(attributes, inputStream);
					}
				}
				catch (IOException e)
				{
					LOG.error("Fout bij laden gescande formulier: " + e.getMessage(), e);
				}
			}
		});

		return response;
	}

	protected URLConnection getUrlConnection() throws IOException
	{
		URL url = new URL(spec);
		return url.openConnection();
	}
}
