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

import java.util.Date;

import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Audited
public class TijdelijkAdres extends ScreenitAdres
{

	private static final long serialVersionUID = 1L;

	@Temporal(TemporalType.TIMESTAMP)
	private Date startDatum;

	@Temporal(TemporalType.TIMESTAMP)
	private Date eindDatum;

	@ManyToOne
	@NotAudited
	private PostcodeCoordinaten postcodeCoordinaten;

	public Date getStartDatum()
	{
		return startDatum;
	}

	public void setStartDatum(Date startDatum)
	{
		this.startDatum = startDatum;
	}

	public Date getEindDatum()
	{
		return eindDatum;
	}

	public void setEindDatum(Date eindDatum)
	{
		this.eindDatum = eindDatum;
	}

	public PostcodeCoordinaten getPostcodeCoordinaten()
	{
		return postcodeCoordinaten;
	}

	public void setPostcodeCoordinaten(PostcodeCoordinaten postcodeCoordinaten)
	{
		this.postcodeCoordinaten = postcodeCoordinaten;
	}

	@Override
	public String toString()
	{
		StringBuilder stringbuilder = new StringBuilder();
		stringbuilder.append("ID: ");
		stringbuilder.append(this.getId());
		stringbuilder.append(", StartDatum: ");
		stringbuilder.append(this.getStartDatum());
		stringbuilder.append(", Einddatum: ");
		stringbuilder.append(this.getEindDatum());
		return stringbuilder.toString();
	}
}
