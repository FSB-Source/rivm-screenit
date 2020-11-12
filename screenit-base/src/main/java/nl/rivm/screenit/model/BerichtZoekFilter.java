package nl.rivm.screenit.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

public class BerichtZoekFilter implements Serializable
{
	private static final long serialVersionUID = 1L;

	private Boolean mldBerichten;

	private Boolean paLabBerichten;

	private Boolean cytologieBerichten;

	private Boolean followUpBerichten;

	private String bsn;

	private String text;

	public ScreeningOrganisatie getScreeningOrganisatie()
	{
		return null;
	}

	public void setScreeningOrganisatie(ScreeningOrganisatie screeningOrganisatie)
	{

	}

	public Boolean getMldBerichten()
	{
		return mldBerichten;
	}

	public void setMldBerichten(Boolean mldBerichten)
	{
		this.mldBerichten = mldBerichten;
	}

	public Boolean getPaLabBerichten()
	{
		return paLabBerichten;
	}

	public void setPaLabBerichten(Boolean paLabBerichten)
	{
		this.paLabBerichten = paLabBerichten;
	}

	public Boolean getCytologieBerichten()
	{
		return cytologieBerichten;
	}

	public void setCytologieBerichten(Boolean cytologieBerichten)
	{
		this.cytologieBerichten = cytologieBerichten;
	}

	public Boolean getFollowUpBerichten()
	{
		return followUpBerichten;
	}

	public void setFollowUpBerichten(Boolean followUpBerichten)
	{
		this.followUpBerichten = followUpBerichten;
	}

	public String getBsn()
	{
		return bsn;
	}

	public void setBsn(String bsn)
	{
		this.bsn = bsn;
	}

	public String getText()
	{
		return text;
	}

	public void setText(String text)
	{
		this.text = text;
	}

}
