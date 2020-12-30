
package nl.rivm.screenit.security;

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

import java.io.Serializable;

import nl.rivm.screenit.model.Account;

public class ScreenitPrincipal implements Serializable
{

	private static final long serialVersionUID = 1L;

	private final Class<? extends Account> accountClass;

	private final Serializable accountId;

	private final Boolean checkBvo;

	public ScreenitPrincipal(Class<? extends Account> accountClass, Serializable accountId)
	{
		this.accountClass = accountClass;
		this.accountId = accountId;
		this.checkBvo = true;
	}

	public ScreenitPrincipal(Class<? extends Account> accountClass, Serializable accountId, boolean checkBvo)
	{
		this.accountClass = accountClass;
		this.accountId = accountId;
		this.checkBvo = checkBvo;
	}

	public Class<? extends Account> getAccountClass()
	{
		return accountClass;
	}

	public Serializable getAccountId()
	{
		return accountId;
	}

	public Boolean getCheckBvo()
	{
		return checkBvo;
	}

	@Override
	public int hashCode()
	{
		final int prime = 31;
		int result = 1;
		result = prime * result + ((accountClass == null) ? 0 : accountClass.hashCode());
		result = prime * result + ((accountId == null) ? 0 : accountId.hashCode());
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
		if (!(obj instanceof ScreenitPrincipal))
		{
			return false;
		}
		ScreenitPrincipal other = (ScreenitPrincipal) obj;
		if (accountClass == null)
		{
			if (other.accountClass != null)
			{
				return false;
			}
		}
		else if (!accountClass.equals(other.accountClass))
		{
			return false;
		}
		if (accountId == null)
		{
			if (other.accountId != null)
			{
				return false;
			}
		}
		else if (!accountId.equals(other.accountId))
		{
			return false;
		}
		return true;
	}
}
