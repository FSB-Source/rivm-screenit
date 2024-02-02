
package nl.rivm.screenit.batch.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.planning.IPlanningFilter;
import nl.topicuszorg.wicket.planning.model.appointment.Location;

public class PlanningFilter implements IPlanningFilter
{

	private Date begintijd;

	private Date eindtijd;

	private Integer maxPerDagPerLocatie = null;

	private List<ColoscopieCentrum> coloscopieCentra;

	private Date datum;

	@Override
	public Date getBegintijd()
	{
		return begintijd;
	}

	public void setBegintijd(Date begintijd)
	{
		this.begintijd = begintijd;
	}

	@Override
	public Date getEindtijd()
	{
		return eindtijd;
	}

	public void setEindtijd(Date eindtijd)
	{
		this.eindtijd = eindtijd;
	}

	@Override
	public Integer getMaxPerDagPerLocatie()
	{
		return maxPerDagPerLocatie;
	}

	public void setMaxPerDagPerLocatie(Integer maxPerDagPerLocatie)
	{
		this.maxPerDagPerLocatie = maxPerDagPerLocatie;
	}

	@Override
	public List<ColoscopieCentrum> getColoscopieCentra()
	{
		return coloscopieCentra;
	}

	public void setColoscopieCentra(List<ColoscopieCentrum> coloscopieCentra)
	{
		this.coloscopieCentra = coloscopieCentra;
	}

	@Override
	public Date getDatum()
	{
		return datum;
	}

	public void setDatum(Date datum)
	{
		this.datum = datum;
	}

	@Override
	public List<Location> getPlanbareLocaties()
	{
		List<Location> list = new ArrayList<Location>();

		if (getColoscopieCentra() != null)
		{
			for (ColoscopieCentrum coloscopieCentrum : getColoscopieCentra())
			{
				if (Boolean.TRUE.equals(coloscopieCentrum.getActief()))
				{
					for (Kamer kamer : coloscopieCentrum.getKamers())
					{
						if (Boolean.TRUE.equals(kamer.getActief()))
						{
							list.add(kamer);
						}
					}
				}
			}
		}

		return list;
	}

}
