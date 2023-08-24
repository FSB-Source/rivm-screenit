package nl.rivm.screenit.mamma.se.proxy.model;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;

import lombok.AccessLevel;
import lombok.EqualsAndHashCode;
import lombok.Getter;

@Getter
@EqualsAndHashCode
public final class OphaalRequest
{
	private final RequestTypeCentraal requestType;

	private final LocalDate datum;

	@Getter(AccessLevel.NONE)
	@EqualsAndHashCode.Exclude
	private final Runnable ophaalActie;

	public OphaalRequest(RequestTypeCentraal requestType, Runnable ophaalActie)
	{
		this(requestType, null, ophaalActie);
	}

	public OphaalRequest(RequestTypeCentraal requestType, LocalDate datum, Runnable ophaalActie)
	{
		this.requestType = requestType;
		this.datum = datum;
		this.ophaalActie = ophaalActie;
	}

	public void run()
	{
		ophaalActie.run();
	}
}
