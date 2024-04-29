package nl.rivm.screenit.model.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.Serializable;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RoosterListViewFilter implements Serializable, Cloneable
{
	private RoosterItemStatus status;

	private Date startDatum;

	private Date eindDatum;

	private LocalTime startTijd;

	private LocalTime eindTijd;

	private List<Integer> dagen = new ArrayList<>();

	private Long kamerId;

	private boolean rekeningHoudenMetCapaciteitMeeBepaald = true;

	public List<Integer> getSqlDagen()
	{
		var clone = new ArrayList<>(dagen);
		var index = clone.indexOf(7);
		if (index != -1)
		{
			clone.set(index, 0);
		}
		return clone;
	}

	@Override
	public RoosterListViewFilter clone()
	{
		try
		{
			return (RoosterListViewFilter) super.clone();
		}
		catch (CloneNotSupportedException e)
		{
			throw new IllegalStateException(e);
		}
	}
}
