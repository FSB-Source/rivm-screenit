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

import java.util.ArrayList;
import java.util.List;

import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Index;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.Hibernate;
import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Proxy;
import org.hibernate.envers.Audited;
import org.hibernate.envers.RelationTargetAuditMode;

@Entity
@Table(schema = "algemeen", name = "org_organisatie_medewerker", indexes = { @Index(name = "idx_organisatie_medewerker_actief", columnList = "actief") })
@Proxy
@Cache(
	usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE,
	region = "organisatie.cache"
)
@Audited
@Getter
@Setter
public class InstellingGebruiker extends AbstractHibernateObject implements Account, IActief
{
	@ManyToOne
	@JoinColumn(nullable = false)
	@Audited(targetAuditMode = RelationTargetAuditMode.NOT_AUDITED)
	private Instelling organisatie;

	@ManyToOne
	@JoinColumn(nullable = false)
	@Audited(targetAuditMode = RelationTargetAuditMode.NOT_AUDITED)
	private Gebruiker medewerker;

	@Column(length = 20)
	private String telefoon;

	@Column(length = 100)
	private String email;

	@Column(length = 50)
	private String aliasnaam;

	@Column(unique = true, nullable = true, length = 10)
	private String uzipasnummer;

	@OneToMany(mappedBy = "instellingGebruiker")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "organisatie.cache")
	private List<InstellingGebruikerRol> rollen = new ArrayList<>();

	@ElementCollection(targetClass = Bevolkingsonderzoek.class)
	@Column(name = "bevolkingsonderzoeken", nullable = true)
	@Enumerated(EnumType.STRING)
	@CollectionTable(schema = "algemeen", name = "instelling_gebruiker_bevolkingsonderzoeken")
	private List<Bevolkingsonderzoek> bevolkingsonderzoeken;

	private Boolean actief;

	@Override
	public int hashCode()
	{
		final int prime = 31;
		int result = 1;
		if (getId() != null)
		{
			result = prime * result + getId().hashCode();
		}
		else
		{
			result = prime * result + (actief == null ? 0 : actief.hashCode());
			result = prime * result + (getMedewerker() == null ? 0 : getMedewerker().hashCode());
			result = prime * result + (getOrganisatie() == null ? 0 : getOrganisatie().hashCode());
		}
		return result;
	}

	@Override
	public boolean equals(Object obj)
	{
		boolean returnValue = true;
		if (obj == null)
		{
			returnValue = false;
		}
		else if (Hibernate.getClass(this) != Hibernate.getClass(obj))
		{
			returnValue = false;
		}
		else
		{
			InstellingGebruiker other = (InstellingGebruiker) obj;
			if (getId() == null)
			{
				if (other.getId() != null)
				{
					returnValue = false;
				}
			}
			else if (!getId().equals(other.getId()))
			{
				returnValue = false;
			}

			if (returnValue && getId() == null)
			{
				if (actief == null)
				{
					if (other.actief != null)
					{
						returnValue = false;
					}
				}
				else if (!actief.equals(other.actief))
				{
					returnValue = false;
				}
				if (getMedewerker() == null)
				{
					if (other.getMedewerker() != null)
					{
						returnValue = false;
					}
				}
				else if (!getMedewerker().equals(other.getMedewerker()))
				{
					returnValue = false;
				}
				if (getOrganisatie() == null)
				{
					if (other.getOrganisatie() != null)
					{
						returnValue = false;
					}
				}
				else if (!getOrganisatie().equals(other.getOrganisatie()))
				{
					returnValue = false;
				}
			}
		}

		return returnValue;
	}

	@Override
	public String toString()
	{
		return "InstellingGebruiker [naam = " + getMedewerker().getNaamVolledig() + "]";
	}

}
