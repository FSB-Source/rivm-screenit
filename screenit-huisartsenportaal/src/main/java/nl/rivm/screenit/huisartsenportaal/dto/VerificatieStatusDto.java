package nl.rivm.screenit.huisartsenportaal.dto;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2016 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

public class VerificatieStatusDto
{
	private Boolean succes;

	private Boolean reedsGeverifieerd = false;

	private String locatieNaam;

	private String zorgmailKlantnummer;

	public String getLocatieNaam()
	{
		return locatieNaam;
	}

	public void setLocatieNaam(String locatieNaam)
	{
		this.locatieNaam = locatieNaam;
	}

	public String getZorgmailKlantnummer()
	{
		return zorgmailKlantnummer;
	}

	public void setZorgmailKlantnummer(String zorgmailKlantnummer)
	{
		this.zorgmailKlantnummer = zorgmailKlantnummer;
	}

	public Boolean getSucces()
	{
		return succes;
	}

	public void setSucces(Boolean succes)
	{
		this.succes = succes;
	}

	public Boolean getReedsGeverifieerd()
	{
		return reedsGeverifieerd;
	}

	public void setReedsGeverifieerd(Boolean reedsGeverifieerd)
	{
		this.reedsGeverifieerd = reedsGeverifieerd;
	}
}
