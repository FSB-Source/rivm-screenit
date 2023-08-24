package nl.rivm.screenit.main.web.gebruiker.screening.cervix.labformulier.controleren;

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
import java.net.URLConnection;
import java.time.Duration;
import java.util.Base64;

import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.ScannedFormulierViewerResourceExternal;

import org.apache.commons.lang3.StringUtils;

public class SpherionFormulierViewerResource extends ScannedFormulierViewerResourceExternal
{

	private final String username;

	private final String password;

	public SpherionFormulierViewerResource(String spec, boolean alsAttachement, String username, String password)
	{
		super(spec, alsAttachement);
		this.username = username;
		this.password = password;
	}

	public SpherionFormulierViewerResource(String spec, boolean alsAttachement, Duration cacheDuration, String username, String password)
	{
		super(spec, alsAttachement, cacheDuration);
		this.username = username;
		this.password = password;
	}

	@Override
	protected URLConnection getUrlConnection() throws IOException
	{
		URLConnection c = super.getUrlConnection();
		if (StringUtils.isNotEmpty(username))
		{
			String userpass = username + ":" + password;
			String basicAuth = "Basic " + new String(Base64.getEncoder().encode(userpass.getBytes()));
			c.setRequestProperty("Authorization", basicAuth);
		}
		return c;
	}
}
