package nl.rivm.screenit.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.ArrayList;
import java.util.List;

public class ZorgmailImportVoortgang
{

	private int nieuweHuisartsen = 0;

	private int geupdateHuisartsen = 0;

	private int geinactiveerdeHuisartsen = 0;

	private int totaalAantalRijen = 0;

	private int geinactiveerdeHuisartsenAfter = 0;

	private List<String> klantnummers = new ArrayList<>();

	public ZorgmailImportVoortgang()
	{
	}

	public int getNieuweHuisartsen()
	{
		return nieuweHuisartsen;
	}

	public void incrNieuweHuisartsen()
	{
		this.nieuweHuisartsen++;
	}

	public int getGeupdateHuisartsen()
	{
		return geupdateHuisartsen;
	}

	public void incrGeupdateHuisartsen()
	{
		this.geupdateHuisartsen++;
	}

	public int getGeinactiveerdeHuisartsen()
	{
		return geinactiveerdeHuisartsen;
	}

	public void incrGeinactiveerdeHuisartsen()
	{
		this.geinactiveerdeHuisartsen++;
	}

	public int getTotaalAantalRijen()
	{
		return totaalAantalRijen;
	}

	public void incrTotaalAantalRijen()
	{
		totaalAantalRijen++;
	}

	public int getGeinactiveerdeHuisartsenAfter()
	{
		return geinactiveerdeHuisartsenAfter;
	}

	public void setGeinactiveerdeHuisartsenAfter(int geinactiveerdeHuisartsenAfter)
	{
		this.geinactiveerdeHuisartsenAfter = geinactiveerdeHuisartsenAfter;
	}

	public void addKlantnummer(String klantnummer)
	{
		getKlantnummers().add(klantnummer);
	}

	public List<String> getKlantnummers()
	{
		return klantnummers;
	}

}
