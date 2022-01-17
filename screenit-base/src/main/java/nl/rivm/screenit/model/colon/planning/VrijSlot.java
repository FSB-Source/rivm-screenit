package nl.rivm.screenit.model.colon.planning;

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

import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.Date;

import net.sf.json.JSONObject;

import nl.rivm.screenit.model.colon.Kamer;

public class VrijSlot implements Comparable<VrijSlot>
{

	private Date startTijd;

	private Date eindTijd;

	private Kamer locatie;

	private Long kamerId;

	private BigDecimal latitude;

	private BigDecimal longitude;

	private int id;

	private Integer maxAantalDeelnemers;

	private Integer aantalDeelnemers;

	private Long roosterItemId;

	public Date getStartTijd()
	{
		return startTijd;
	}

	public void setStartTijd(Date startTijd)
	{
		this.startTijd = startTijd;
	}

	public Date getEindTijd()
	{
		return eindTijd;
	}

	public void setEindTijd(Date eindTijd)
	{
		this.eindTijd = eindTijd;
	}

	public Kamer getLocatie()
	{
		return locatie;
	}

	public void setLocatie(Kamer locatie)
	{
		this.locatie = locatie;
	}

	public String getEindTijdAsString()
	{
		if (startTijd == null || eindTijd == null)
		{
			return "Handmatig";
		}
		return new SimpleDateFormat("HH:mm").format(eindTijd);
	}

	public String getStartTijdAsString()
	{
		if (startTijd == null || eindTijd == null)
		{
			return "Handmatig";
		}
		return new SimpleDateFormat("HH:mm").format(startTijd);
	}

	public String getDatumAsString()
	{
		if (startTijd == null)
		{
			return "";
		}
		return new SimpleDateFormat("dd-MM-yyyy").format(startTijd);
	}

	public int getId()
	{
		return id;
	}

	public void setId(int id)
	{
		this.id = id;
	}

	public JSONObject toJSONObject()
	{
		JSONObject jsonObject = new JSONObject();

		jsonObject.element("id", getId());
		jsonObject.element("start", startTijd.getTime());
		jsonObject.element("end", eindTijd.getTime());
		jsonObject.element("maxAantalDeelnemers", maxAantalDeelnemers);
		jsonObject.element("aantalDeelnemers", aantalDeelnemers);
		jsonObject.element("type", "'free'");

		return jsonObject;
	}

	@Override
	public int hashCode()
	{
		final int prime = 31;
		int result = 1;
		result = prime * result + (eindTijd == null ? 0 : eindTijd.hashCode());
		result = prime * result + (locatie == null ? 0 : locatie.hashCode());
		result = prime * result + (kamerId == null ? 0 : kamerId.hashCode());
		result = prime * result + (startTijd == null ? 0 : startTijd.hashCode());
		result = prime * result + (roosterItemId == null ? 0 : roosterItemId.hashCode());
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
		if (locatie == null)
		{
			if (other.locatie != null)
			{
				return false;
			}
		}
		else if (!locatie.equals(other.locatie))
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
		if (roosterItemId == null)
		{
			if (other.roosterItemId != null)
			{
				return false;
			}
		}
		else if (!roosterItemId.equals(other.roosterItemId))
		{
			return false;
		}
		if (startTijd == null)
		{
			if (other.startTijd != null)
			{
				return false;
			}
		}
		else if (!startTijd.equals(other.startTijd))
		{
			return false;
		}
		return true;
	}

	public Integer getMaxAantalDeelnemers()
	{
		return maxAantalDeelnemers;
	}

	public void setMaxAantalDeelnemers(Integer maxAantalDeelnemers)
	{
		this.maxAantalDeelnemers = maxAantalDeelnemers;
	}

	public Integer getAantalDeelnemers()
	{
		return aantalDeelnemers;
	}

	public void setAantalDeelnemers(Integer aantalDeelnemers)
	{
		this.aantalDeelnemers = aantalDeelnemers;
	}

	@Override
	public String toString()
	{
		String string = "VrijSlot [startDatum=" + getDatumAsString() + ", startTijd=" + getStartTijdAsString() + ", eindTijd=" + getEindTijdAsString();
		if (locatie != null)
		{
			string += ", locatie=" + locatie.getName() + " van IL " + locatie.getColoscopieCentrum().getNaam();
		}
		else
		{
			string += ", locatie=" + kamerId + " van IL " + latitude + "/" + longitude;
		}
		string += "]";
		return string;

	}

	@Override
	public int compareTo(VrijSlot o)
	{
		return this.getStartTijd().compareTo(o.getStartTijd());
	}

	public Long getKamerId()
	{
		return kamerId;
	}

	public void setKamerId(Long kamerId)
	{
		this.kamerId = kamerId;
	}

	public BigDecimal getLatitude()
	{
		return latitude;
	}

	public void setLatitude(BigDecimal latitude)
	{
		this.latitude = latitude;
	}

	public BigDecimal getLongitude()
	{
		return longitude;
	}

	public void setLongitude(BigDecimal longitude)
	{
		this.longitude = longitude;
	}

	public Long getRoosterItemId()
	{
		return roosterItemId;
	}

	public void setRoosterItemId(Long roosterItemId)
	{
		this.roosterItemId = roosterItemId;
	}
}
