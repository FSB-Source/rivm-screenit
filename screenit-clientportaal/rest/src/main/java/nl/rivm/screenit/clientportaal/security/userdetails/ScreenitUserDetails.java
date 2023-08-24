package nl.rivm.screenit.clientportaal.security.userdetails;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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
import java.util.Collection;

import lombok.Getter;

import nl.rivm.screenit.model.Client;

import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UsernameNotFoundException;

public class ScreenitUserDetails implements UserDetails
{
	private final String bsn;

	@Getter
	private final Long clientId;

	public ScreenitUserDetails(Client client)
	{
		if (client == null)
		{
			throw new UsernameNotFoundException("Geen client met deze BSN gevonden");
		}
		this.bsn = client.getPersoon().getBsn();
		this.clientId = client.getId();
	}

	@Override
	public Collection<? extends GrantedAuthority> getAuthorities()
	{
		return new ArrayList<>();
	}

	@Override
	public String getPassword()
	{
		return "";
	}

	@Override
	public String getUsername()
	{
		return bsn;
	}

	@Override
	public boolean isAccountNonExpired()
	{
		return true;
	}

	@Override
	public boolean isAccountNonLocked()
	{
		return true;
	}

	@Override
	public boolean isCredentialsNonExpired()
	{
		return true;
	}

	@Override
	public boolean isEnabled()
	{
		return true;
	}
}
