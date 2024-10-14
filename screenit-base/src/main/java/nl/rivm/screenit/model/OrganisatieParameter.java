package nl.rivm.screenit.model;

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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "algemeen", uniqueConstraints = @UniqueConstraint(name = "uk_organisatie_parameter", columnNames = { "organisatie", "key" }))
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "organisatie.cache")
@Audited
public class OrganisatieParameter extends AbstractHibernateObject
{
	@ManyToOne(fetch = FetchType.EAGER, optional = false)
	private Instelling organisatie;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private OrganisatieParameterKey key;

	@Column(length = 2048)
	private String value;

	@Transient
	private transient String parameterNaam;

	public Instelling getOrganisatie()
	{
		return organisatie;
	}

	public void setOrganisatie(Instelling organisatie)
	{
		this.organisatie = organisatie;
	}

	public OrganisatieParameterKey getKey()
	{
		return key;
	}

	public void setKey(OrganisatieParameterKey key)
	{
		this.key = key;
	}

	public String getValue()
	{
		return value;
	}

	public void setValue(String value)
	{
		this.value = value;
	}

	@Transient
	public String getParameterNaam()
	{
		return parameterNaam;
	}

	@Transient
	public void setParameterNaam(String parameterNaam)
	{
		this.parameterNaam = parameterNaam;
	}
}
