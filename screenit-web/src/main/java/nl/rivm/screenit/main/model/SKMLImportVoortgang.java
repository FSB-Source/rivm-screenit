package nl.rivm.screenit.main.model;

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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;

public class SKMLImportVoortgang
{

	private Date start;

	private Date eind;

	private int nieuw;

	private int geupdate;

	private int verwijderd;

	private List<String> foutmeldingen;

	public SKMLImportVoortgang()
	{
		this.nieuw = 0;
		this.geupdate = 0;
		this.verwijderd = 0;
		this.foutmeldingen = new ArrayList<>();
	}

	public Date getStart()
	{
		return start;
	}

	public void setStart(Date start)
	{
		this.start = start;
	}

	public Date getEind()
	{
		return eind;
	}

	public void setEind(Date eind)
	{
		this.eind = eind;
	}

	public int getNieuw()
	{
		return nieuw;
	}

	public void setNieuw(int nieuw)
	{
		this.nieuw = nieuw;
	}

	public int getGeupdate()
	{
		return geupdate;
	}

	public void setGeupdate(int geupdate)
	{
		this.geupdate = geupdate;
	}

	public int getVerwijderd()
	{
		return verwijderd;
	}

	public void setVerwijderd(int verwijderd)
	{
		this.verwijderd = verwijderd;
	}

	public List<String> getFoutmeldingen()
	{
		return foutmeldingen;
	}

	public void setFoutmeldingen(List<String> foutmeldingen)
	{
		this.foutmeldingen = foutmeldingen;
	}

	@Override
	public String toString()
	{
		StringBuilder sb = new StringBuilder();
		sb.append("Resultaat van import, Nieuw: ");
		sb.append(getNieuw());
		sb.append(", Geupdate: ");
		sb.append(getGeupdate());
		sb.append(", Verwijderd: ");
		sb.append(getVerwijderd());
		sb.append(".");
		if (CollectionUtils.isNotEmpty(getFoutmeldingen()))
		{
			sb.append(" Foutmeldingen: ");
			for (String fout : getFoutmeldingen())
			{
				sb.append(fout);
				sb.append(", ");
			}
		}
		return StringUtils.removeEnd(sb.toString(), ", ");
	}
}
