package nl.rivm.screenit.mamma.planning.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.util.UUID;

public class PlanningConceptEntiteit extends PlanningEntiteit
{
	private final UUID conceptId;

	public PlanningConceptEntiteit(Long id)
	{
		super(id);
		this.conceptId = UUID.randomUUID();
	}

	public UUID getConceptId()
	{
		return conceptId;
	}

	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof PlanningConceptEntiteit))
		{
			return false;
		}

		PlanningConceptEntiteit that = (PlanningConceptEntiteit) o;
		return conceptId.equals(that.conceptId);
	}

	@Override
	public int hashCode()
	{
		return conceptId.hashCode();
	}
}
