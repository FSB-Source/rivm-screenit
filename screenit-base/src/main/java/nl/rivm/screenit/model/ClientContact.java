
package nl.rivm.screenit.model;

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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "algemeen")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public class ClientContact extends AbstractHibernateObject
{
	
	private static final long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.EAGER, optional = false)
	private Client client;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = false)
	private Date datum;

	@ManyToOne(fetch = FetchType.EAGER, optional = false)
	private InstellingGebruiker instellingGebruiker;

	@Column(length = HibernateMagicNumber.L2048)
	private String opmerking;

	@OneToMany(mappedBy = "contact", cascade = CascadeType.ALL)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<ClientContactActie> acties = new ArrayList<>();

	public Client getClient()
	{
		return client;
	}

	public void setClient(Client client)
	{
		this.client = client;
	}

	public Date getDatum()
	{
		return datum;
	}

	public void setDatum(Date datum)
	{
		this.datum = datum;
	}

	public InstellingGebruiker getInstellingGebruiker()
	{
		return instellingGebruiker;
	}

	public void setInstellingGebruiker(InstellingGebruiker instellingGebruiker)
	{
		this.instellingGebruiker = instellingGebruiker;
	}

	public String getOpmerking()
	{
		return opmerking;
	}

	public void setOpmerking(String opmerking)
	{
		this.opmerking = opmerking;
	}

	public List<ClientContactActie> getActies()
	{
		return acties;
	}

	public void setActies(List<ClientContactActie> acties)
	{
		this.acties = acties;
	}
}
