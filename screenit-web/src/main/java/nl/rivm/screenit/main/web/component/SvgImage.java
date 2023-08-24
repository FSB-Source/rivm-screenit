package nl.rivm.screenit.main.web.component;

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
import java.time.Duration;

import org.apache.commons.io.IOUtils;
import org.apache.wicket.markup.html.image.NonCachingImage;
import org.apache.wicket.request.resource.DynamicImageResource;
import org.apache.wicket.request.resource.IResource.Attributes;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class SvgImage extends NonCachingImage
{
	private static final Logger LOG = LoggerFactory.getLogger(SvgImage.class);

	public SvgImage(String id)
	{
		super(id);
		setImageResource(new DynamicImageResource("svg")
		{
			@Override
			protected byte[] getImageData(Attributes attributes)
			{
				try
				{
					return IOUtils.toByteArray(getSvgImageData(attributes));
				}
				catch (Exception e)
				{
					LOG.error("Fout bij aanmaken van svg.", e);
				}
				return null;
			}

			@Override
			protected void configureResponse(ResourceResponse response, Attributes attributes)
			{
				super.configureResponse(response, attributes);
				response.setContentType("image/svg+xml");
				response.setFileName("image_" + System.currentTimeMillis() + ".svg");
				response.setCacheDuration(Duration.ofSeconds(1));
			}
		});
	}

	protected abstract InputStream getSvgImageData(Attributes attributes) throws IOException;
}
