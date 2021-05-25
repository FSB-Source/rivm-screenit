
package nl.rivm.screenit.model.formulieren;

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

public class PalgaNumber extends Number
{

	private static final long serialVersionUID = 1L;

	private Integer waarde;

	private String stringValue;

	public PalgaNumber(Integer waarde, String stringValue)
	{
		this.waarde = waarde;
		this.stringValue = stringValue;
	}

	@Override
	public int intValue()
	{
		return waarde;
	}

	@Override
	public long longValue()
	{
		return waarde;
	}

	@Override
	public float floatValue()
	{
		return waarde;
	}

	@Override
	public double doubleValue()
	{
		return waarde;
	}

	public Integer getWaarde()
	{
		return waarde;
	}

	public void setWaarde(Integer waarde)
	{
		this.waarde = waarde;
	}

	public String getStringValue()
	{
		return stringValue;
	}

	public void setStringValue(String stringValue)
	{
		this.stringValue = stringValue;
	}

	@Override
	public int hashCode()
	{
		final int prime = 31;
		int result = 1;
		result = prime * result + ((stringValue == null) ? 0 : stringValue.hashCode());
		result = prime * result + ((waarde == null) ? 0 : waarde.hashCode());
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
		if (!(obj instanceof PalgaNumber))
		{
			return false;
		}
		PalgaNumber other = (PalgaNumber) obj;
		if (stringValue == null)
		{
			if (other.stringValue != null)
			{
				return false;
			}
		}
		else if (!stringValue.equals(other.stringValue))
		{
			return false;
		}
		if (waarde == null)
		{
			if (other.waarde != null)
			{
				return false;
			}
		}
		else if (!waarde.equals(other.waarde))
		{
			return false;
		}
		return true;
	}
}
