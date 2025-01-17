package nl.rivm.screenit.mamma.se.proxy.model;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;
import java.time.LocalDateTime;

import org.dcm4che3.data.PersonName;

import com.fasterxml.jackson.annotation.JsonIgnore;

public class ClientScreenITWerklijstItem extends ScreenITWerklijstItem
{
	private long uitnodigingsNr;

	private LocalDateTime startDatumTijd;

	private String voorletters;

	private String tussenvoegsel;

	private String achternaam;

	private String bsn;

	private LocalDate geboortedatum;

	private String geslacht;

	public long getUitnodigingsNr()
	{
		return uitnodigingsNr;
	}

	public void setUitnodigingsNr(long uitnodigingsNr)
	{
		this.uitnodigingsNr = uitnodigingsNr;
	}

	@JsonIgnore
	public PersonName getPersonName()
	{
		PersonName name = new PersonName();
		name.set(PersonName.Component.GivenName, voorletters);
		name.set(PersonName.Component.MiddleName, tussenvoegsel);
		name.set(PersonName.Component.FamilyName, achternaam);

		return name;
	}

	public String getVoorletters()
	{
		return voorletters;
	}

	public void setVoorletters(String voorletters)
	{
		this.voorletters = voorletters;
	}

	public String getTussenvoegsel()
	{
		return tussenvoegsel;
	}

	public void setTussenvoegsel(String tussenvoegsel)
	{
		this.tussenvoegsel = tussenvoegsel;
	}

	public String getAchternaam()
	{
		return achternaam;
	}

	public void setAchternaam(String achternaam)
	{
		this.achternaam = achternaam;
	}

	public String getBsn()
	{
		return bsn;
	}

	public void setBsn(String bsn)
	{
		this.bsn = bsn;
	}

	public LocalDate getGeboortedatum()
	{
		return geboortedatum;
	}

	public void setGeboortedatum(LocalDate geboortedatum)
	{
		this.geboortedatum = geboortedatum;
	}

	public String getGeslacht()
	{
		return geslacht;
	}

	public void setGeslacht(String geslacht)
	{
		this.geslacht = geslacht;
	}

	public LocalDateTime getStartDatumTijd()
	{
		return startDatumTijd;
	}

	public void setStartDatumTijd(LocalDateTime startDatumTijd)
	{
		this.startDatumTijd = startDatumTijd;
	}
}
