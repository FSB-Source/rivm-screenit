
package nl.rivm.screenit.model.project;

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

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.IActief;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(schema = "gedeeld")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public class ProjectClient extends AbstractHibernateObject implements IActief
{

	private static final long serialVersionUID = 1L;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date toegevoegd;

	@Cascade({ CascadeType.SAVE_UPDATE })
	@ManyToOne(fetch = FetchType.LAZY)
	private Client client;

	@Cascade({ CascadeType.SAVE_UPDATE })
	@ManyToOne(fetch = FetchType.LAZY)
	private Project project;

	@Cascade({ CascadeType.SAVE_UPDATE })
	@ManyToOne(fetch = FetchType.LAZY)
	private ProjectGroep groep;

	@Cascade({ CascadeType.ALL })
	@OneToMany(fetch = FetchType.LAZY, mappedBy = "projectClient")
	private List<ProjectClientAttribuut> attributen = new ArrayList<ProjectClientAttribuut>();

	@Cascade({ CascadeType.ALL })
	@OneToMany(fetch = FetchType.LAZY, mappedBy = "projectClient")
	private List<ProjectBrief> brieven = new ArrayList<ProjectBrief>();

	private Boolean actief = Boolean.TRUE;

	@Enumerated(EnumType.STRING)
	private ProjectInactiefReden projectInactiefReden;

	@Temporal(TemporalType.TIMESTAMP)
	private Date projectInactiefDatum;

	@Cascade({ CascadeType.ALL })
	@OneToOne(fetch = FetchType.LAZY, optional = true)
	@NotAudited
	private ProjectInactiveerDocument projectInactiveerDocument;

	private Boolean isUitgenodigdInProjectPeriode = Boolean.FALSE;

	public ProjectClient()
	{
	}

	public ProjectClient(Client client, ProjectGroep groep)
	{
		this.client = client;
		this.groep = groep;
		this.project = groep.getProject();
	}

	public Client getClient()
	{
		return client;
	}

	public void setClient(Client client)
	{
		this.client = client;
	}

	public Project getProject()
	{
		return project;
	}

	public void setProject(Project project)
	{
		this.project = project;
	}

	public ProjectGroep getGroep()
	{
		return groep;
	}

	public void setGroep(ProjectGroep groep)
	{
		this.groep = groep;
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

	public Date getToegevoegd()
	{
		return toegevoegd;
	}

	public void setToegevoegd(Date toegevoegd)
	{
		this.toegevoegd = toegevoegd;
	}

	public ProjectInactiefReden getProjectInactiefReden()
	{
		return projectInactiefReden;
	}

	public void setProjectInactiefReden(ProjectInactiefReden projectInactiefReden)
	{
		this.projectInactiefReden = projectInactiefReden;
	}

	public Date getProjectInactiefDatum()
	{
		return projectInactiefDatum;
	}

	public void setProjectInactiefDatum(Date projectInactiefDatum)
	{
		this.projectInactiefDatum = projectInactiefDatum;
	}

	public List<ProjectBrief> getBrieven()
	{
		return brieven;
	}

	public void setBrieven(List<ProjectBrief> brieven)
	{
		this.brieven = brieven;
	}

	public ProjectInactiveerDocument getProjectInactiveerDocument()
	{
		return projectInactiveerDocument;
	}

	public void setProjectInactiveerDocument(ProjectInactiveerDocument projectInactiveerDocument)
	{
		this.projectInactiveerDocument = projectInactiveerDocument;
	}

	public List<ProjectClientAttribuut> getAttributen()
	{
		return attributen;
	}

	public void setAttributen(List<ProjectClientAttribuut> attributen)
	{
		this.attributen = attributen;
	}

	public Boolean getUitgenodigdInProjectPeriode()
	{
		return isUitgenodigdInProjectPeriode;
	}

	public void setUitgenodigdInProjectPeriode(Boolean uitgenodigdInProjectPeriode)
	{
		isUitgenodigdInProjectPeriode = uitgenodigdInProjectPeriode;
	}
}
