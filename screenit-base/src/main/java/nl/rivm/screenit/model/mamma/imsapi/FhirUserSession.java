package nl.rivm.screenit.model.mamma.imsapi;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import com.fasterxml.jackson.annotation.JsonInclude;

public class FhirUserSession
{
	private String resourceType = "UserSession";

	private FhirIdentifiableEntity user;

	private FhirFocus focus;

	private FhirContext context;

	@JsonInclude(JsonInclude.Include.NON_NULL)
	private FhirStatus status;

	public FhirIdentifiableEntity getUser()
	{
		return user;
	}

	public void setUser(FhirIdentifiableEntity user)
	{
		this.user = user;
	}

	public FhirFocus getFocus()
	{
		return focus;
	}

	public void setFocus(FhirFocus focus)
	{
		this.focus = focus;
	}

	public FhirContext getContext()
	{
		return context;
	}

	public void setContext(FhirContext context)
	{
		this.context = context;
	}

	public String getResourceType()
	{
		return resourceType;
	}

	public FhirStatus getStatus()
	{
		return status;
	}

	public void setStatus(FhirStatus status)
	{
		this.status = status;
	}
}
