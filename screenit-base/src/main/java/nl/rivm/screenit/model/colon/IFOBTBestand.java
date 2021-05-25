
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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.colon.enums.IFOBTBestandStatus;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "colon")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class IFOBTBestand extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@Enumerated(EnumType.STRING)
	private IFOBTBestandStatus status;

	@Temporal(TemporalType.TIMESTAMP)
	private Date statusDatum;

	@ManyToOne
	private IFobtLaboratorium laboratorium;

	@OneToMany(mappedBy = "bestand")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<IFOBTUitslag> uitslagen = new ArrayList<>();

	private String instumentId;

	private String naamBestand;

	private String pathBestand;

	private Integer aantalVerwerkt = Integer.valueOf(0);

	private Integer aantalControleUitslagen = Integer.valueOf(0);

	public IFOBTBestandStatus getStatus()
	{
		return status;
	}

	public void setStatus(IFOBTBestandStatus status)
	{
		this.status = status;
	}

	public Date getStatusDatum()
	{
		return statusDatum;
	}

	public void setStatusDatum(Date statusDatum)
	{
		this.statusDatum = statusDatum;
	}

	public IFobtLaboratorium getLaboratorium()
	{
		return laboratorium;
	}

	public void setLaboratorium(IFobtLaboratorium laboratorium)
	{
		this.laboratorium = laboratorium;
	}

	public String getInstumentId()
	{
		return instumentId;
	}

	public void setInstumentId(String instumentId)
	{
		this.instumentId = instumentId;
	}

	public String getNaamBestand()
	{
		return naamBestand;
	}

	public void setNaamBestand(String naamBestand)
	{
		this.naamBestand = naamBestand;
	}

	public String getPathBestand()
	{
		return pathBestand;
	}

	public void setPathBestand(String pathBestand)
	{
		this.pathBestand = pathBestand;
	}

	public Integer getAantalVerwerkt()
	{
		return aantalVerwerkt;
	}

	public void setAantalVerwerkt(Integer aantalVerwerkt)
	{
		this.aantalVerwerkt = aantalVerwerkt;
	}

	public Integer getAantalControleUitslagen()
	{
		return aantalControleUitslagen;
	}

	public void setAantalControleUitslagen(Integer aantalControleUitslagen)
	{
		this.aantalControleUitslagen = aantalControleUitslagen;
	}

	public List<IFOBTUitslag> getUitslagen()
	{
		return uitslagen;
	}

	public void setUitslagen(List<IFOBTUitslag> uitslagen)
	{
		this.uitslagen = uitslagen;
	}

}
