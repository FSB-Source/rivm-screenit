package nl.rivm.screenit.mamma.se.proxy.model;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import lombok.Getter;

public enum RequestTypeCentraal
{
	GET_MAMMOGRAFEN("/mammografen"),
	GET_HUISARTSEN("/huisarts/all"),
	RESET_HUISARTSEN_CACHE("/huisarts/resetCache"),
	GET_ZORGINSTELLINGEN("/zorginstelling/metMammapoliOfRadiologie"),
	RESET_ZORGINSTELLINGEN_CACHE("/zorginstelling/resetCache"),
	GET_DAGLIJST("/daglijst"),
	GET_PLANNING("/planning"),
	POST_STATUS("/status");

	@Getter
	private final String pathPostfix;

	RequestTypeCentraal(String pathPostfix)
	{
		this.pathPostfix = pathPostfix;
	}
}
