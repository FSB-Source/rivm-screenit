package nl.rivm.screenit.model.envers;

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
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.topicuszorg.hibernate.object.model.HibernateObject;

import org.hibernate.envers.RevisionEntity;
import org.hibernate.envers.RevisionNumber;
import org.hibernate.envers.RevisionTimestamp;

@Entity
@Table(schema = "gedeeld")
@RevisionEntity(ScreenitRevisionListener.class)
public class ScreenitRevisionEntity implements HibernateObject
{
	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue
	@RevisionNumber
	private Long id;

	@RevisionTimestamp
	private long timestamp;

	@ManyToOne
	private Gebruiker gebruiker;

	@ManyToOne
	private InstellingGebruiker instellingGebruiker;

	@ManyToOne
	private Client client;

	@Column(nullable = true, length = HibernateMagicNumber.L255)
	private String opmerking;

	@Override
	public Long getId()
	{
		return id;
	}

	public void setId(Long id)
	{
		this.id = id;
	}

	public long getTimestamp()
	{
		return timestamp;
	}

	public void setTimestamp(long timestamp)
	{
		this.timestamp = timestamp;
	}

	public Gebruiker getGebruiker()
	{
		return gebruiker;
	}

	public void setGebruiker(Gebruiker gebruiker)
	{
		this.gebruiker = gebruiker;
	}

	public Client getClient()
	{
		return client;
	}

	public void setClient(Client client)
	{
		this.client = client;
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
}
