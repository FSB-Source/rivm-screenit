package nl.rivm.screenit.main.web;

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

import javax.servlet.http.HttpServletRequest;

import org.apache.wicket.protocol.http.CsrfPreventionRequestCycleListener;
import org.apache.wicket.request.http.WebRequest;
import org.apache.wicket.util.string.Strings;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ScreenITCsrfRequestCycleListener extends CsrfPreventionRequestCycleListener
{
	private static final Logger LOG = LoggerFactory
		.getLogger(ScreenITCsrfRequestCycleListener.class);

	protected String getSourceUri(HttpServletRequest containerRequest)
	{
		String sourceUri = containerRequest.getHeader(WebRequest.HEADER_ORIGIN);
		if (Strings.isEmpty(sourceUri))
		{
			sourceUri = containerRequest.getHeader(WebRequest.HEADER_REFERER);
		}

		if (sourceUri != null && sourceUri.startsWith("https") && "http".equals(containerRequest.getScheme()))
		{
			sourceUri = sourceUri.replace("https", "http");
			LOG.debug("Https vervangen voor http:" + sourceUri);
		}

		return normalizeUri(sourceUri);
	}
}
