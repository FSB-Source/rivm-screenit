package nl.rivm.screenit.mamma.se.dao;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import nl.rivm.screenit.model.mamma.enums.MammaIdentificatiesoort;

public class ClientIdentificatie
{

	private final MammaIdentificatiesoort soort;

	private final String nummer;

	public ClientIdentificatie(MammaIdentificatiesoort soort, String nummer)
	{
		this.soort = soort;
		this.nummer = nummer;
	}

	public MammaIdentificatiesoort getSoort()
	{
		return soort;
	}

	public String getNummer()
	{
		return nummer;
	}
}
