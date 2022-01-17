package nl.rivm.screenit.huisartsenportaal.model;

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

import java.io.Serializable;

import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.MappedSuperclass;

import org.hibernate.Hibernate;
import org.hibernate.envers.Audited;

@Audited
@MappedSuperclass
public abstract class AbstractReferenceObject implements Serializable
{
	private static final long serialVersionUID = 1L;

	@Id
	@Access(AccessType.PROPERTY)
	@GeneratedValue(strategy = GenerationType.AUTO)
	private Long huisartsportaalId;

	@Access(AccessType.PROPERTY)
	private Long screenitId;

	public Long getHuisartsportaalId()
	{
		return huisartsportaalId;
	}

	public void setHuisartsportaalId(Long huisartsportaalId)
	{
		this.huisartsportaalId = huisartsportaalId;
	}

	public Long getScreenitId()
	{
		return screenitId;
	}

	public void setScreenitId(Long screenitId)
	{
		this.screenitId = screenitId;
	}

	@Override
	public int hashCode()
	{
		final int prime = 31;
		int result = 1;

		if (getHuisartsportaalId() != null)
		{
			result = prime * result + getHuisartsportaalId().hashCode();
		}
		else
		{
			result = super.hashCode();
		}

		return result;
	}

	@Override
	public boolean equals(Object obj)
	{
		boolean returnValue = true;
		if (obj == null)
		{
			returnValue = false;
		}
		else if (Hibernate.getClass(this) != Hibernate.getClass(obj))
		{
			returnValue = false;
		}
		else
		{
			AbstractReferenceObject other = (AbstractReferenceObject) obj;
			if (getHuisartsportaalId() == null)
			{
				if (other.getHuisartsportaalId() != null)
				{
					returnValue = false;
				}
			}
			else if (!getHuisartsportaalId().equals(other.getHuisartsportaalId()))
			{
				returnValue = false;
			}

			if (returnValue && getHuisartsportaalId() == null)
			{
				returnValue = super.equals(obj);
			}
		}

		return returnValue;
	}
}
