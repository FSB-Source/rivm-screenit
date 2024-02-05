package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Objects;

import nl.rivm.screenit.model.InstellingGebruiker;

class DistributedLockKey
{
	private String locknaam;

	private InstellingGebruiker gebruiker;

	DistributedLockKey(String locknaam, InstellingGebruiker gebruiker)
	{
		this.locknaam = locknaam;
		this.gebruiker = gebruiker;
	}

	DistributedLockKey(String locknaam)
	{
		this.locknaam = locknaam;
	}

	String getLocknaam()
	{
		return locknaam;
	}

	InstellingGebruiker getGebruiker()
	{
		return gebruiker;
	}

	@Override
	public String toString()
	{
		return locknaam + (gebruiker == null ? "" : " - " + gebruiker.getMedewerker().getGebruikersnaam());
	}

	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (o == null || getClass() != o.getClass())
		{
			return false;
		}
		DistributedLockKey that = (DistributedLockKey) o;
		return Objects.equals(locknaam, that.locknaam) &&
			Objects.equals(gebruiker, that.gebruiker);
	}

	@Override
	public int hashCode()
	{
		return Objects.hash(locknaam, gebruiker);
	}
}
