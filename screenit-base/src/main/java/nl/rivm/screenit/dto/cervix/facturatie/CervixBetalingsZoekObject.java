package nl.rivm.screenit.dto.cervix.facturatie;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.Serializable;
import java.util.Date;

import nl.rivm.screenit.model.ScreeningOrganisatie;

public class CervixBetalingsZoekObject implements Serializable
{

	private Long screeningOrganisatieId;

	private String monsterId;

	private Date geboortedatum;

	private String bsn;

	private String postcode;

	private Integer huisnummer;

	private Date verrichtingsdatumTotEnMet;

	private boolean verrichtingenLaboratorium;

	private boolean verrichtingenHuisarts;

	public Long getScreeningOrganisatieId()
	{
		return screeningOrganisatieId;
	}

	public void setScreeningOrganisatieId(Long screeningOrganisatieId)
	{
		this.screeningOrganisatieId = screeningOrganisatieId;
	}

	public void setScreeningOrganisatie(ScreeningOrganisatie screeningOrganisatie)
	{
		this.screeningOrganisatieId = screeningOrganisatie.getId();
	}

	public String getMonsterId()
	{
		return monsterId;
	}

	public void setMonsterId(String monsterId)
	{
		this.monsterId = monsterId;
	}

	public Date getGeboortedatum()
	{
		return geboortedatum;
	}

	public void setGeboortedatum(Date geboortedatum)
	{
		this.geboortedatum = geboortedatum;
	}

	public String getBsn()
	{
		return bsn;
	}

	public void setBsn(String bsn)
	{
		this.bsn = bsn;
	}

	public String getPostcode()
	{
		return postcode;
	}

	public void setPostcode(String postcode)
	{
		this.postcode = postcode;
	}

	public Date getVerrichtingsdatumTotEnMet()
	{
		return verrichtingsdatumTotEnMet;
	}

	public void setVerrichtingsdatumTotEnMet(Date verrichtingsdatumTotEnMet)
	{
		this.verrichtingsdatumTotEnMet = verrichtingsdatumTotEnMet;
	}

	public boolean isVerrichtingenLaboratorium()
	{
		return verrichtingenLaboratorium;
	}

	public void setVerrichtingenLaboratorium(boolean verrichtingenLaboratorium)
	{
		this.verrichtingenLaboratorium = verrichtingenLaboratorium;
	}

	public boolean isVerrichtingenHuisarts()
	{
		return verrichtingenHuisarts;
	}

	public void setVerrichtingenHuisarts(boolean verrichtingenHuisarts)
	{
		this.verrichtingenHuisarts = verrichtingenHuisarts;
	}

	public void setHuisnummer(Integer huisnummer)
	{
		this.huisnummer = huisnummer;
	}

	public Integer getHuisnummer()
	{
		return huisnummer;
	}

}
