package nl.rivm.screenit.model.mamma.imsapi;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

public class FhirSyncStatus
{
	private String syncError;

	private FhirIdentifiableEntity syncCode;

	private FhirIdentifiableEntity source;

	public void setSyncError(String syncError)
	{
		this.syncError = syncError;
	}

	public String getSyncError()
	{
		return syncError;
	}

	public void setSyncCode(FhirIdentifiableEntity syncCode)
	{
		this.syncCode = syncCode;
	}

	public FhirIdentifiableEntity getSyncCode()
	{
		return syncCode;
	}

	public void setSource(FhirIdentifiableEntity source)
	{
		this.source = source;
	}

	public FhirIdentifiableEntity getSource()
	{
		return source;
	}
}
