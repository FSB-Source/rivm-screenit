package nl.rivm.screenit.main.web.gebruiker.testen.postcode;

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

import java.io.Serializable;

public class TestPostcode implements Serializable
{

	private static final long serialVersionUID = 1L;

	private String orgLatitude;

	private String orgLongitude;

	private String clientLatitude;

	private String clientLongitude;

	public String getOrgLatitude()
	{
		return orgLatitude;
	}

	public void setOrgLatitude(String orgLatitude)
	{
		this.orgLatitude = orgLatitude;
	}

	public String getOrgLongitude()
	{
		return orgLongitude;
	}

	public void setOrgLongitude(String orgLongitude)
	{
		this.orgLongitude = orgLongitude;
	}

	public String getClientLatitude()
	{
		return clientLatitude;
	}

	public void setClientLatitude(String clientLatitude)
	{
		this.clientLatitude = clientLatitude;
	}

	public String getClientLongitude()
	{
		return clientLongitude;
	}

	public void setClientLongitude(String clientLongitude)
	{
		this.clientLongitude = clientLongitude;
	}

}
