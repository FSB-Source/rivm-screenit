
package nl.rivm.screenit.batch.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.model.colon.planning.VrijSlot;

import org.apache.commons.lang.builder.HashCodeBuilder;
import org.drools.planner.api.domain.solution.PlanningEntityCollectionProperty;
import org.drools.planner.core.score.buildin.hardandsoft.HardAndSoftScore;
import org.drools.planner.core.solution.Solution;

public class IntakeSolution implements Solution<HardAndSoftScore>
{
	private List<VrijSlot> vrijeSloten;

	private List<ClientAfspraak> clientAfspraken;

	private HardAndSoftScore score;

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

	@Override
	public HardAndSoftScore getScore()
	{
		return score;
	}

	@Override
	public void setScore(HardAndSoftScore score)
	{
		this.score = score;
	}

	@Override
	public Collection<? extends Object> getProblemFacts()
	{
		List<Object> facts = new ArrayList<>();
		facts.addAll(vrijeSloten);
		return facts;
	}

	@Override
	public Solution<HardAndSoftScore> cloneSolution()
	{
		IntakeSolution clone = new IntakeSolution();
		clone.vrijeSloten = vrijeSloten;
		List<ClientAfspraak> clonedClientAfspraken = new ArrayList<>(clientAfspraken.size());
		for (ClientAfspraak clientAfspraak : clientAfspraken)
		{
			ClientAfspraak clonedClientAfspraak = clientAfspraak.clone();
			clonedClientAfspraken.add(clonedClientAfspraak);
		}
		clone.clientAfspraken = clonedClientAfspraken;
		clone.score = score;
		return clone;
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
			for (Iterator<ClientAfspraak> it = clientAfspraken.iterator(), otherIt = other.clientAfspraken.iterator(); it.hasNext();)
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
