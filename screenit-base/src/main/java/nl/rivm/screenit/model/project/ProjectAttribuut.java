
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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.IActief;
import nl.rivm.screenit.model.INaam;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;

@Entity
@Audited
@Table(schema = "algemeen", uniqueConstraints = @UniqueConstraint(name = "uc_project_attribuut_naam_project", columnNames = { "naam", "project" }))
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class ProjectAttribuut extends AbstractHibernateObject implements INaam, IActief
{

	private static final long serialVersionUID = 1L;

	@Column(nullable = false)
	private String naam;

	@Column(nullable = false)
	private Boolean actief = true;

	@Column(nullable = false)
	private Boolean nietZichtbaarInClientDossier = false;

	@Cascade({ CascadeType.SAVE_UPDATE })
	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private Project project;

	@Column(nullable = false)
	private String mergeField;

	public ProjectAttribuut()
	{
	}

	@Override
	public String getNaam()
	{
		return naam;
	}

	public void setNaam(String naam)
	{
		this.naam = naam;
	}

	public Project getProject()
	{
		return project;
	}

	public void setProject(Project project)
	{
		this.project = project;
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

	public Boolean getNietZichtbaarInClientDossier()
	{
		return nietZichtbaarInClientDossier;
	}

	public void setNietZichtbaarInClientDossier(Boolean nietZichtbaarInClientDossier)
	{
		this.nietZichtbaarInClientDossier = nietZichtbaarInClientDossier;
	}

	public String getMergeField()
	{
		return mergeField;
	}

	public void setMergeField(String mergeField)
	{
		this.mergeField = mergeField;
	}
}
