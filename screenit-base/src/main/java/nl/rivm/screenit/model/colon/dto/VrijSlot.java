package nl.rivm.screenit.model.colon.dto;

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

import java.math.BigDecimal;
import java.util.Date;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.colon.planning.ColonIntakekamer;
import nl.rivm.screenit.util.DateUtil;

@Setter
@Getter
public class VrijSlot implements Comparable<VrijSlot>
{
	private Date startTijd;

	private Date eindTijd;

	private ColonIntakekamer kamer;

	private Long kamerId;

	private BigDecimal latitude;

	private BigDecimal longitude;

	private int id;

	private Integer maxAantalDeelnemers;

	private Integer aantalDeelnemers;

	private Long afspraakslotId;

	public String getEindTijdAsString()
	{
		if (startTijd == null || eindTijd == null)
		{
			return "Handmatig";
		}
		return DateUtil.formatTime(eindTijd);
	}

	public String getStartTijdAsString()
	{
		if (startTijd == null || eindTijd == null)
		{
			return "Handmatig";
		}
		return DateUtil.formatTime(startTijd);
	}

	public String getDatumAsString()
	{
		if (startTijd == null)
		{
			return "";
		}
		return DateUtil.formatShortDate(startTijd);
	}

	@Override
	public int hashCode()
	{
		final int prime = 31;
		int result = 1;
		result = prime * result + (eindTijd == null ? 0 : eindTijd.hashCode());
		result = prime * result + (kamer == null ? 0 : kamer.hashCode());
		result = prime * result + (kamerId == null ? 0 : kamerId.hashCode());
		result = prime * result + (startTijd == null ? 0 : startTijd.hashCode());
		result = prime * result + (afspraakslotId == null ? 0 : afspraakslotId.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj)
	{
		if (this == obj)
		{
			return true;
		}
		if (obj == null)
		{
			return false;
		}
		if (getClass() != obj.getClass())
		{
			return false;
		}
		VrijSlot other = (VrijSlot) obj;
		if (eindTijd == null)
		{
			if (other.eindTijd != null)
			{
				return false;
			}
		}
		else if (!eindTijd.equals(other.eindTijd))
		{
			return false;
		}
		if (kamer == null)
		{
			if (other.kamer != null)
			{
				return false;
			}
		}
		else if (!kamer.equals(other.kamer))
		{
			return false;
		}
		if (kamerId == null)
		{
			if (other.kamerId != null)
			{
				return false;
			}
		}
		else if (!kamerId.equals(other.kamerId))
		{
			return false;
		}
		if (afspraakslotId == null)
		{
			if (other.afspraakslotId != null)
			{
				return false;
			}
		}
		else if (!afspraakslotId.equals(other.afspraakslotId))
		{
			return false;
		}
		if (startTijd == null)
		{
			return other.startTijd == null;
		}
		else
		{
			return startTijd.equals(other.startTijd);
		}
	}

	@Override
	public String toString()
	{
		String string = "VrijSlot [startDatum=" + getDatumAsString() + ", startTijd=" + getStartTijdAsString() + ", eindTijd=" + getEindTijdAsString();
		if (kamer != null)
		{
			string += ", kamer=" + kamer.getNaam() + " van IL " + kamer.getIntakelocatie().getNaam();
		}
		else
		{
			string += ", kamer=" + kamerId + " van IL " + latitude + "/" + longitude;
		}
		string += "]";
		return string;

	}

	@Override
	public int compareTo(VrijSlot o)
	{
		return this.getStartTijd().compareTo(o.getStartTijd());
	}

}
