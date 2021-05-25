package nl.rivm.screenit.model.mamma.enums;

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

public enum MammaHL7v24ORMBerichtStatus
{

	SCHEDULED("Scheduled"),
	STARTED("Started"),
	COMPLETED("Completed"),
	REPORTED("Reported"),
	AUTHORISED("Authorised"),
	CANCELLED("Cancelled"),
	CENTRALAVAILABLE("CentralAvailable"),
	GOINGTODELETE("GoingToDelete"),
	DELETE("Delete"),
	DELETED("Deleted"),
	ERROR("Error");

	private String label;

	public String getLabel()
	{
		return label;
	}

	public static MammaHL7v24ORMBerichtStatus getEnumForLabel(String label)
	{
		for (MammaHL7v24ORMBerichtStatus status : values())
		{
			if (status.getLabel().equals(label))
			{
				return status;
			}
		}
		throw new IllegalStateException("Enum niet gevonden");
	}

	MammaHL7v24ORMBerichtStatus(String label)
	{
		this.label = label;
	}
}
