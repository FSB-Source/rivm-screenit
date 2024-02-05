package nl.rivm.screenit.model.mamma.enums;

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

import nl.rivm.screenit.model.INaam;

public enum MammaLezingType implements INaam
{
	EERSTE_LEZING("1ste lezing"),

	TWEEDE_LEZING("2de lezing"),

	DISCREPANTIE_LEZING("Discrepantielezing"),

	ARBITRAGE_LEZING("Arbitragelezing"),

	VERSLAG_LEZING("Verslaglezing");

	private String naam;

	MammaLezingType(String naam)
	{
		this.naam = naam;
	}

	@Override
	public String getNaam()
	{
		return naam;
	}
}
