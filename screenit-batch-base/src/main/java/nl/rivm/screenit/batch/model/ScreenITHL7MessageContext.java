package nl.rivm.screenit.batch.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import lombok.Getter;
import lombok.Setter;

import ca.uhn.hl7v2.app.Connection;

@Getter
@Setter
public class ScreenITHL7MessageContext
{
	private final HapiContextType type;

	private String host;

	private Integer port;

	private final HL7v24ResponseWrapper responseWrapper = new HL7v24ResponseWrapper();

	private Connection connection = null;

	public ScreenITHL7MessageContext(HapiContextType type)
	{
		this.type = type;
	}
}
