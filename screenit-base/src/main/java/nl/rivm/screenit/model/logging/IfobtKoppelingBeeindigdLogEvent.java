package nl.rivm.screenit.model.logging;

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

import javax.persistence.Entity;
import javax.persistence.Table;

import org.apache.commons.lang.StringUtils;

@Entity
@Table(schema = "gedeeld")
public class IfobtKoppelingBeeindigdLogEvent extends LogEvent
{
	private Integer aantalIfobtenVerwerkt = 0;

	public Integer getAantalIfobtenVerwerkt()
	{
		return aantalIfobtenVerwerkt;
	}

	public void setAantalIfobtenVerwerkt(Integer aantalIfobtenVerwerkt)
	{
		this.aantalIfobtenVerwerkt = aantalIfobtenVerwerkt;
	}

	@Override
	public String getMelding()
	{
		String melding = super.getMelding();
		if (StringUtils.isBlank(melding))
		{
			melding = "#" + aantalIfobtenVerwerkt + " verwerkt";
		}
		return melding;
	}
}
