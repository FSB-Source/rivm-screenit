package nl.rivm.screenit.model.colon;

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

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.IActief;
import nl.topicuszorg.wicket.planning.model.appointment.Location;

import org.hibernate.Hibernate;
import org.hibernate.envers.Audited;

@Entity
@Audited
@Getter
@Setter
public class Kamer extends Location implements IActief
{
	private Boolean actief;

	@ManyToOne(fetch = FetchType.EAGER)
	private ColoscopieCentrum coloscopieCentrum;

	@Override
	public int hashCode()
	{
		final int prime = 31;
		int result = 1;
		result = prime * result + (getId() == null ? 0 : getId().hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj)
	{
		if (this == obj)
		{
			return true;
		}
		if (obj == null)
		{
			return false;
		}
		if (Hibernate.getClass(this) != Hibernate.getClass(obj))
		{
			return false;
		}
		Kamer other = (Kamer) obj;
		if (getId() == null)
		{
			return other.getId() == null;
		}
		else
		{
			return getId().equals(other.getId());
		}
	}
}
