package nl.rivm.screenit.batch.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.model.colon.dto.VrijSlot;

import org.apache.commons.lang.builder.HashCodeBuilder;
import org.optaplanner.core.api.domain.solution.PlanningEntityCollectionProperty;
import org.optaplanner.core.api.domain.solution.PlanningScore;
import org.optaplanner.core.api.domain.solution.PlanningSolution;
import org.optaplanner.core.api.domain.solution.ProblemFactCollectionProperty;
import org.optaplanner.core.api.domain.valuerange.ValueRangeProvider;
import org.optaplanner.core.api.score.buildin.hardsoft.HardSoftScore;

@PlanningSolution
public class IntakeSolution
{
	private List<VrijSlot> vrijeSloten;

	private List<ClientAfspraak> clientAfspraken;

	private HardSoftScore score;

	@ValueRangeProvider(id = "vrijeSlotenRange")
	@ProblemFactCollectionProperty
	public List<VrijSlot> getVrijeSloten()
	{
		return vrijeSloten;
	}

	public void setVrijeSloten(List<VrijSlot> vrijeSloten)
	{
		this.vrijeSloten = vrijeSloten;
	}

	@PlanningEntityCollectionProperty
	public List<ClientAfspraak> getClientAfspraken()
	{
		return clientAfspraken;
	}

	public void setClientAfspraken(List<ClientAfspraak> clientAfspraken)
	{
		this.clientAfspraken = clientAfspraken;
	}

	@PlanningScore
	public HardSoftScore getScore()
	{
		return score;
	}

	public void setScore(HardSoftScore score)
	{
		this.score = score;
	}

	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof IntakeSolution))
		{
			return false;
		}
		else
		{
			IntakeSolution other = (IntakeSolution) o;
			if (clientAfspraken.size() != other.clientAfspraken.size())
			{
				return false;
			}
			for (Iterator<ClientAfspraak> it = clientAfspraken.iterator(), otherIt = other.clientAfspraken.iterator(); it.hasNext(); )
			{
				ClientAfspraak clientAfspraak = it.next();
				ClientAfspraak otherClientAfspraak = otherIt.next();

				if (!clientAfspraak.solutionEquals(otherClientAfspraak))
				{
					return false;
				}
			}
			return true;
		}
	}

	@Override
	public int hashCode()
	{
		HashCodeBuilder hashCodeBuilder = new HashCodeBuilder();
		for (ClientAfspraak clientAfspraak : clientAfspraken)
		{

			hashCodeBuilder.append(clientAfspraak.solutionHashCode());
		}
		return hashCodeBuilder.toHashCode();
	}

	@Override
	public String toString()
	{
		return "IntakeSolution [vrijeSloten=" + vrijeSloten + ", clientAfspraken=" + clientAfspraken + ", score=" + score + "]";
	}
}
