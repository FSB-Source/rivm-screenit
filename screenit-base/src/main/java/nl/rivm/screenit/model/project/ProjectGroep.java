package nl.rivm.screenit.model.project;

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

import java.util.Date;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.IActief;
import nl.rivm.screenit.model.INaam;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(schema = "algemeen")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public class ProjectGroep extends AbstractHibernateObject implements INaam, IActief
{

	private static final long serialVersionUID = 1L;

	private String naam;

	private Boolean actief;

	@Temporal(TemporalType.TIMESTAMP)
	private Date actiefDatum;

	private Integer populatie;

	@Enumerated(EnumType.STRING)
	private GroepInvoer groepInvoer;

	@NotAudited
	@Cascade({ CascadeType.ALL })
	@OneToOne(fetch = FetchType.LAZY, mappedBy = "groep")
	private ProjectImport projectImport;

	@Cascade({ CascadeType.ALL })
	@OneToMany(fetch = FetchType.LAZY, mappedBy = "groep")
	private List<ProjectBestand> projectBestanden;

	@Cascade({ CascadeType.SAVE_UPDATE })
	@ManyToOne(fetch = FetchType.LAZY)
	private Project project;

	@Cascade({ CascadeType.ALL })
	@OneToMany(fetch = FetchType.LAZY, mappedBy = "groep")
	private List<ProjectClient> clienten;

	@Temporal(TemporalType.DATE)
	private Date uitnodigenVoorDKvoor;

	@Column(nullable = true)
	@Temporal(TemporalType.DATE)
	private Date uitnodigingenPushenNa;

	public ProjectGroep()
	{
	}

	public ProjectGroep(Project project)
	{
		this.project = project;
	}

	@Override
	public String getNaam()
	{
		return naam;
	}

	@Override
	public Boolean getActief()
	{
		return actief;
	}

	@Override
	public void setActief(Boolean actief)
	{
		this.actief = actief;
	}

	public GroepInvoer getGroepInvoer()
	{
		return groepInvoer;
	}

	public void setGroepInvoer(GroepInvoer groepInvoer)
	{
		this.groepInvoer = groepInvoer;
	}

	public Project getProject()
	{
		return project;
	}

	public void setProject(Project project)
	{
		this.project = project;
	}

	public List<ProjectClient> getClienten()
	{
		return clienten;
	}

	public void setClienten(List<ProjectClient> clienten)
	{
		this.clienten = clienten;
	}

	public void setNaam(String naam)
	{
		this.naam = naam;
	}

	public ProjectImport getProjectImport()
	{
		return projectImport;
	}

	public void setProjectImport(ProjectImport projectImport)
	{
		this.projectImport = projectImport;
	}

	public Integer getPopulatie()
	{
		return populatie;
	}

	public void setPopulatie(Integer populatie)
	{
		this.populatie = populatie;
	}

	public Date getActiefDatum()
	{
		return actiefDatum;
	}

	public void setActiefDatum(Date actiefDatum)
	{
		this.actiefDatum = actiefDatum;
	}

	public Date getUitnodigenVoorDKvoor()
	{
		return uitnodigenVoorDKvoor;
	}

	public void setUitnodigenVoorDKvoor(Date uitnodigenVoorDKvoor)
	{
		this.uitnodigenVoorDKvoor = uitnodigenVoorDKvoor;
	}

	public Date getUitnodigingenPushenNa()
	{
		return uitnodigingenPushenNa;
	}

	public void setUitnodigingenPushenNa(Date uitnodigingenPushenNa)
	{
		this.uitnodigingenPushenNa = uitnodigingenPushenNa;
	}
}
