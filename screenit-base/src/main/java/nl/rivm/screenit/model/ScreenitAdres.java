package nl.rivm.screenit.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.organisatie.model.Adres;

import org.hibernate.Hibernate;

public abstract class ScreenitAdres extends Adres
{

	@Override
	public int hashCode()
	{
		final int prime = 31;
		int result = 1;

		if (getId() != null)
		{
			result = prime * result + getId().hashCode();
		}
		else
		{
			result = concreateHashCode(prime, result);
		}

		return result;
	}

	protected int concreateHashCode(int prime, int result)
	{
		return super.hashCode();
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
			HibernateObject other = (HibernateObject) obj;
			if (getId() == null)
			{
				if (other.getId() != null)
				{
					returnValue = false;
				}
			}
			else if (!getId().equals(other.getId()))
			{
				returnValue = false;
			}

			if (returnValue && getId() == null)
			{
				returnValue = concreateEquals(other);
			}
		}

		return returnValue;
	}

	protected boolean concreateEquals(HibernateObject obj)
	{
		return super.equals(obj);
	}
}
