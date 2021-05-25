
package nl.rivm.screenit.model.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.colon.enums.IFOBTUitslagType;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "colon")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class IFOBTUitslag extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@Enumerated(EnumType.STRING)
	private IFOBTUitslagType type;

	private String barcode;

	@Temporal(TemporalType.TIMESTAMP)
	private Date analyseDatum;

	private BigDecimal uitslag;

	@ManyToOne
	private IFOBTBestand bestand;

	public IFOBTUitslagType getType()
	{
		return type;
	}

	public void setType(IFOBTUitslagType type)
	{
		this.type = type;
	}

	public String getBarcode()
	{
		return barcode;
	}

	public void setBarcode(String barcode)
	{
		this.barcode = barcode;
	}

	public Date getAnalyseDatum()
	{
		return analyseDatum;
	}

	public void setAnalyseDatum(Date analyseDatum)
	{
		this.analyseDatum = analyseDatum;
	}

	public BigDecimal getUitslag()
	{
		return uitslag;
	}

	public void setUitslag(BigDecimal uitslag)
	{
		this.uitslag = uitslag;
	}

	public IFOBTBestand getBestand()
	{
		return bestand;
	}

	public void setBestand(IFOBTBestand bestand)
	{
		this.bestand = bestand;
	}

}
