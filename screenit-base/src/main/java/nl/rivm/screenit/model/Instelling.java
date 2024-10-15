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
import java.util.Date;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.Index;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenInstellingOvereenkomst;
import nl.topicuszorg.organisatie.model.Organisatie;

import org.hibernate.Hibernate;
import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Setter
@Getter
@Entity
@Table(indexes = { @Index(name = "idx_instelling_agbcode", columnList = "agbcode"), @Index(name = "idx_instelling_actief", columnList = "actief") })
@Audited
public class Instelling extends Organisatie<InstellingGebruiker> implements IActief
{
	private Boolean actief = Boolean.TRUE;

	@Column(length = HibernateMagicNumber.L20)
	private String telefoon2;

	@Column(length = HibernateMagicNumber.L200)
	private String website;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private OrganisatieType organisatieType;

	@NotAudited
	@ManyToOne(fetch = FetchType.LAZY)
	private Instelling parent;

	@NotAudited
	@ManyToOne(fetch = FetchType.LAZY)
	private Instelling regio;

	@NotAudited
	@OneToMany(mappedBy = "parent", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "organisatie.cache")
	private List<Instelling> children = new ArrayList<>();

	@NotAudited
	@Column(length = HibernateMagicNumber.L512, unique = true)
	private String rootOid;

	@ManyToOne
	@NotAudited
	private Gebruiker gemachtigde;

	@OneToMany(fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "organisatie.cache")
	@JoinTable(schema = "gedeeld", name = "org_organisatie_documents", joinColumns = { @JoinColumn(name = "org_organisatie") })
	private List<UploadDocument> documents;

	@ManyToOne
	@NotAudited
	private Gebruiker contactPersoon;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "instelling")
	@NotAudited
	private List<AfgeslotenInstellingOvereenkomst> afgeslotenOvereenkomsten;

	@Column(nullable = true)
	@Temporal(TemporalType.TIMESTAMP)
	private Date mammaRadiologieGebeld;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "organisatie")
	@NotAudited
	private List<OrganisatieParameter> parameters = new ArrayList<>();

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
			result = prime * result + (getNaam() == null ? 0 : getNaam().hashCode());
			result = prime * result + (getAgbcode() == null ? 0 : getAgbcode().hashCode());
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
			Instelling other = (Instelling) obj;
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
				if (getNaam() == null)
				{
					if (other.getNaam() != null)
					{
						returnValue = false;
					}
				}
				else if (!getNaam().equals(other.getNaam()))
				{
					returnValue = false;
				}
				if (getAgbcode() == null)
				{
					if (other.getAgbcode() != null)
					{
						returnValue = false;
					}
				}
				else if (!getAgbcode().equals(other.getAgbcode()))
				{
					returnValue = false;
				}
			}
		}

		return returnValue;
	}

}
