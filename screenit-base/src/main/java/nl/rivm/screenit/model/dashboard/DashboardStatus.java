package nl.rivm.screenit.model.dashboard;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.enums.Level;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "gedeeld", uniqueConstraints = { @UniqueConstraint(name = "dashboardOrganisatie", columnNames = { "type", "organisatie" }) })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class DashboardStatus extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private DashboardType type;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private Instelling organisatie;

	@Column(length = 100)
	private String emailadressen;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private Level level;

	public Level getLevel()
	{
		return level;
	}

	public void setLevel(Level level)
	{
		this.level = level;
	}

	public DashboardType getType()
	{
		return type;
	}

	public void setType(DashboardType type)
	{
		this.type = type;
	}

	public Instelling getOrganisatie()
	{
		return organisatie;
	}

	public void setOrganisatie(Instelling organisatie)
	{
		this.organisatie = organisatie;
	}

	public String getEmailadressen()
	{
		return emailadressen;
	}

	public void setEmailadressen(String emailadressen)
	{
		this.emailadressen = emailadressen;
	}
}
