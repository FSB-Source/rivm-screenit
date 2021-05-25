package nl.rivm.screenit.batch.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import java.math.BigDecimal;
import java.util.Date;

import nl.rivm.screenit.batch.VrijSlotComparator;
import nl.rivm.screenit.model.colon.planning.VrijSlot;
import nl.rivm.screenit.util.BigDecimalUtil;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.joda.time.DateTime;
import org.joda.time.Interval;
import org.optaplanner.core.api.domain.entity.PlanningEntity;
import org.optaplanner.core.api.domain.variable.PlanningVariable;

@PlanningEntity
public class ClientAfspraak implements Cloneable
{
	private Long clientId;

	private Long colonScreeningRondeId;

	private Long intakeAfspraakId;

	private VrijSlot vrijSlot;

	private Date analyseDatum;

	private BigDecimal latitude;

	private BigDecimal longitude;

	private double wachttijdNormering;

	private double afstandNormering;

	private Integer cachedWachttijd = null;

	private Integer cachedAfstand = null;

	private Integer defaultAfstand;

	private double distance;

	@PlanningVariable(strengthComparatorClass = VrijSlotComparator.class, valueRangeProviderRefs = { "vrijeSlotenRange" })
	public VrijSlot getVrijSlot()
	{
		return vrijSlot;
	}

	public void setVrijSlot(VrijSlot vrijSlot)
	{
		this.vrijSlot = vrijSlot;
		cachedWachttijd = null;
		cachedAfstand = null;
	}

	public Long getClientId()
	{
		return clientId;
	}

	public void setClientId(Long clientId)
	{
		this.clientId = clientId;
	}

	public Date getAnalyseDatum()
	{
		return analyseDatum;
	}

	public void setAnalyseDatum(Date analyseDatum)
	{
		this.analyseDatum = analyseDatum;
	}

	public boolean solutionEquals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		else if (o instanceof ClientAfspraak)
		{
			ClientAfspraak other = (ClientAfspraak) o;
			return new EqualsBuilder().append(vrijSlot, other.vrijSlot).isEquals();
		}
		else
		{
			return false;
		}
	}

	public int solutionHashCode()
	{
		return new HashCodeBuilder().append(clientId).append(vrijSlot).toHashCode();
	}

	@Override
	protected ClientAfspraak clone()
	{
		ClientAfspraak clone = new ClientAfspraak();
		clone.vrijSlot = vrijSlot;
		clone.clientId = clientId;
		clone.analyseDatum = analyseDatum;
		clone.latitude = latitude;
		clone.longitude = longitude;
		clone.colonScreeningRondeId = colonScreeningRondeId;
		clone.setIntakeAfspraakId(intakeAfspraakId);
		clone.wachttijdNormering = wachttijdNormering;
		clone.afstandNormering = afstandNormering;
		clone.defaultAfstand = defaultAfstand;
		clone.distance = distance;
		return clone;
	}

	@Override
	public String toString()
	{
		return "ClientAfspraak [id=" + clientId + ", vrijSlot=" + vrijSlot + "]";
	}

	public Integer getWachttijd()
	{
		if (vrijSlot == null)
		{
			return 0;
		}
		if (cachedWachttijd != null)
		{
			return cachedWachttijd;
		}

		long wachttijd = new Interval(new DateTime(analyseDatum), new DateTime(vrijSlot.getStartTijd())).toDuration().getStandardHours();

		cachedWachttijd = (int) Math.pow(wachttijd * wachttijdNormering, 2.0);
		return cachedWachttijd;
	}

	public Integer getAfstand()
	{
		if (vrijSlot == null)
		{
			return 0;
		}
		if (cachedAfstand != null)
		{
			return cachedAfstand;
		}
		BigDecimal latitudeTo = vrijSlot.getLatitude();
		BigDecimal longitudeTo = vrijSlot.getLongitude();
		if (latitudeTo == null || latitude == null)
		{
			cachedAfstand = (int) (Math.pow(defaultAfstand * afstandNormering, 2.0));
			return cachedAfstand;
		}

		distance = BigDecimalUtil.berekenDistance(latitude, longitude, latitudeTo, longitudeTo);

		cachedAfstand = (int) (Math.pow(distance * afstandNormering, 2.0));
		return cachedAfstand;
	}

	public Long getColonScreeningRondeId()
	{
		return colonScreeningRondeId;
	}

	public void setColonScreeningRondeId(Long colonScreeningRondeId)
	{
		this.colonScreeningRondeId = colonScreeningRondeId;
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

	public void setWachttijdNormering(double wachttijdNormering)
	{
		this.wachttijdNormering = wachttijdNormering;
	}

	public void setAfstandNormering(double afstandNormering)
	{
		this.afstandNormering = afstandNormering;
	}

	public void setDefaultAfstand(Integer defaultAfstand)
	{
		this.defaultAfstand = defaultAfstand;
	}

	public Long getIntakeAfspraakId()
	{
		return intakeAfspraakId;
	}

	public void setIntakeAfspraakId(Long intakeAfspraakId)
	{
		this.intakeAfspraakId = intakeAfspraakId;
	}

	public double getDistance()
	{
		return distance;
	}
}
