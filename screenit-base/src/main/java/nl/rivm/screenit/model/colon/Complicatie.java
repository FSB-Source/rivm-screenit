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

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.enums.ComplicatieErnst;
import nl.rivm.screenit.model.enums.ComplicatieMoment;
import nl.rivm.screenit.model.enums.ComplicatieSoort;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "colon")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public class Complicatie extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private ComplicatieSoort soort;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private ComplicatieErnst ernst;

	@Enumerated(EnumType.STRING)
	@Column(nullable = true)
	private ComplicatieMoment moment;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date datum;

	@Column(nullable = false)
	private boolean actief = true;

	@ManyToOne(fetch = FetchType.LAZY)
	private MdlVerslag mdlverslag;

	private Boolean handmatig;

	@ManyToOne
	private InstellingGebruiker instellingGebruiker;

	@ManyToOne
	private Client client;

	public ComplicatieSoort getSoort()
	{
		return soort;
	}

	public void setSoort(ComplicatieSoort soort)
	{
		this.soort = soort;
	}

	public ComplicatieErnst getErnst()
	{
		return ernst;
	}

	public void setErnst(ComplicatieErnst ernst)
	{
		this.ernst = ernst;
	}

	public Date getDatum()
	{
		return datum;
	}

	public void setDatum(Date datum)
	{
		this.datum = datum;
	}

	public Client getClient()
	{
		return client;
	}

	public void setClient(Client client)
	{
		this.client = client;
	}

	public MdlVerslag getMdlverslag()
	{
		return mdlverslag;
	}

	public void setMdlverslag(MdlVerslag mdlverslag)
	{
		this.mdlverslag = mdlverslag;
	}

	public ComplicatieMoment getMoment()
	{
		return moment;
	}

	public void setMoment(ComplicatieMoment moment)
	{
		this.moment = moment;
	}

	public InstellingGebruiker getInstellingGebruiker()
	{
		return instellingGebruiker;
	}

	public void setInstellingGebruiker(InstellingGebruiker instellingGebruiker)
	{
		this.instellingGebruiker = instellingGebruiker;
	}

	public boolean isActief()
	{
		return actief;
	}

	public void setActief(boolean actief)
	{
		this.actief = actief;
	}

	public Boolean getHandmatig()
	{
		return handmatig;
	}

	public void setHandmatig(Boolean handmatig)
	{
		this.handmatig = handmatig;
	}

}
