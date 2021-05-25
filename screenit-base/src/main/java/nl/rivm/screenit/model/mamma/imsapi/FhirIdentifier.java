package nl.rivm.screenit.model.mamma.imsapi;

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

import com.fasterxml.jackson.annotation.JsonInclude;

public class FhirIdentifier
{
	private String system;

	private String value;

	@JsonInclude(JsonInclude.Include.NON_NULL)
	private FhirCodableConcept type;

	public FhirIdentifier(String system, String value)
	{
		this.system = system;
		this.value = value;
	}

	public FhirIdentifier()
	{
	}

	public String getSystem()
	{
		return system;
	}

	public void setSystem(String system)
	{
		this.system = system;
	}

	public String getValue()
	{
		return value;
	}

	public void setValue(String value)
	{
		this.value = value;
	}

	public FhirCodableConcept getType()
	{
		return type;
	}

	public void setType(FhirCodableConcept type)
	{
		this.type = type;
	}
}
