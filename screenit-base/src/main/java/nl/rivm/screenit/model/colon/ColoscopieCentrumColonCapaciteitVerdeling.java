
package nl.rivm.screenit.model.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.UitnodigingsGebied;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "colon")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "organisatie.cache")
public class ColoscopieCentrumColonCapaciteitVerdeling extends AbstractHibernateObject
{
	
	private static final long serialVersionUID = 1L;

	@ManyToOne
	private UitnodigingsGebied uitnodigingsGebied;

	@ManyToOne
	private ColoscopieCentrum coloscopieCentrum;

	@Column(nullable = false)
	private Integer percentageCapaciteit;

	@Column(nullable = false)
	private Integer percentageAdherentie;

	public UitnodigingsGebied getUitnodigingsGebied()
	{
		return uitnodigingsGebied;
	}

	public void setUitnodigingsGebied(UitnodigingsGebied uitnodigingsGebied)
	{
		this.uitnodigingsGebied = uitnodigingsGebied;
	}

	public Integer getPercentageCapaciteit()
	{
		return percentageCapaciteit;
	}

	public void setPercentageCapaciteit(Integer percentageCapaciteit)
	{
		this.percentageCapaciteit = percentageCapaciteit;
	}

	public ColoscopieCentrum getColoscopieCentrum()
	{
		return coloscopieCentrum;
	}

	public void setColoscopieCentrum(ColoscopieCentrum coloscopieCentrum)
	{
		this.coloscopieCentrum = coloscopieCentrum;
	}

	public Integer getPercentageAdherentie()
	{
		return percentageAdherentie;
	}

	public void setPercentageAdherentie(Integer percentageAdherentie)
	{
		this.percentageAdherentie = percentageAdherentie;
	}

	@Override
	protected boolean concreateEquals(AbstractHibernateObject obj)
	{
		ColoscopieCentrumColonCapaciteitVerdeling other = (ColoscopieCentrumColonCapaciteitVerdeling) obj;
		if (getUitnodigingsGebied() == null)
		{
			if (other.getUitnodigingsGebied() != null)
			{
				return false;
			}
		}
		else if (!getUitnodigingsGebied().equals(other.getUitnodigingsGebied()))
		{
			return false;
		}
		if (getColoscopieCentrum() == null)
		{
			if (other.getColoscopieCentrum() != null)
			{
				return false;
			}
		}
		else if (!getColoscopieCentrum().equals(other.getColoscopieCentrum()))
		{
			return false;
		}
		return super.concreateEquals(obj);
	}
}
