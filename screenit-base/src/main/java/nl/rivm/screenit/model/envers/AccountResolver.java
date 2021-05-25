
package nl.rivm.screenit.model.envers;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.model.Account;

public class AccountResolver
{

	private static List<AccountResolverDelegate> delegates = new ArrayList<>();

	private AccountResolver()
	{
	}

	public static Account getAccount()
	{
		for (AccountResolverDelegate delegate : delegates)
		{
			if (delegate.getAccount() != null)
			{
				return delegate.getAccount();
			}
		}

		return null;
	}

	public static void registerDelegate(AccountResolverDelegate delegate)
	{
		delegates.add(delegate);
	}

	public interface AccountResolverDelegate
	{
		Account getAccount();
	}
}
