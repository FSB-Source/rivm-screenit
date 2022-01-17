package nl.rivm.screenit.model.mamma.imsapi;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonTypeInfo;

public class FhirContext
{
	@JsonInclude(JsonInclude.Include.NON_NULL)
	@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.WRAPPER_OBJECT)
	private List<FhirWorklistItem> worklist;

	private String type;

	private String value;

	@JsonInclude(JsonInclude.Include.NON_NULL)
	private FhirLayoutImages layoutImages;

	@JsonInclude(JsonInclude.Include.NON_NULL)
	private FhirSyncStatus syncStatus;

	public List<FhirWorklistItem> getWorklist()
	{
		return worklist;
	}

	public FhirLayoutImages getLayoutImages()
	{
		return layoutImages;
	}

	public void setLayoutImages(FhirLayoutImages layoutImages)
	{
		this.layoutImages = layoutImages;
	}

	public void setWorklist(List<FhirWorklistItem> worklist)
	{
		this.worklist = worklist;
	}

	public String getType()
	{
		return type;
	}

	public void setType(String type)
	{
		this.type = type;
	}

	public String getValue()
	{
		return value;
	}

	public void setValue(String value)
	{
		this.value = value;
	}

	public void setSyncStatus(FhirSyncStatus syncStatus)
	{
		this.syncStatus = syncStatus;
	}

	public FhirSyncStatus getSyncStatus()
	{
		return syncStatus;
	}
}
