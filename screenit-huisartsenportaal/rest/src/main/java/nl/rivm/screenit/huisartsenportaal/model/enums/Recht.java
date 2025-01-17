package nl.rivm.screenit.huisartsenportaal.model.enums;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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

import lombok.AllArgsConstructor;

import org.springframework.security.core.GrantedAuthority;

@AllArgsConstructor
public enum Recht implements GrantedAuthority
{

	ROLE_AANVRAGEN("ROLE_AANVRAGEN"),

	ROLE_REGISTEREN("ROLE_REGISTEREN"),

	ROLE_OVEREENKOMST("ROLE_OVEREENKOMST");

	private final String authority;

	@Override
	public String getAuthority()
	{
		return authority;
	}
}
