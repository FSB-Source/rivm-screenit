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

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.Transient;

import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Audited
@Table(schema = "algemeen")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class ProjectBestand extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@Cascade({ CascadeType.ALL })
	@OneToOne(optional = false, fetch = FetchType.LAZY)
	private UploadDocument uploadDocument;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date uploadDatum;

	@Cascade({ CascadeType.SAVE_UPDATE })
	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private Project project;

	@Cascade({ CascadeType.SAVE_UPDATE })
	@ManyToOne(optional = true, fetch = FetchType.LAZY)
	private ProjectGroep groep;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private BestandStatus status;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date statusDatum;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private ProjectBestandType type;

	private boolean populatie = false;

	private boolean attributen = false;

	@NotAudited
	@OneToOne(fetch = FetchType.LAZY, optional = true, mappedBy = "projectBestand")
	@Cascade({ CascadeType.ALL }) 
	private ProjectBestandVerwerking verwerking;

	@Column(nullable = true)
	private String dynamischeInactiveerReden;

	@Transient
	private String toepassenOp;

	public UploadDocument getUploadDocument()
	{
		return uploadDocument;
	}

	public void setUploadDocument(UploadDocument uploadDocument)
	{
		this.uploadDocument = uploadDocument;
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

	public BestandStatus getStatus()
	{
		return status;
	}

	public void setStatus(BestandStatus status)
	{
		this.status = status;
	}

	public Date getStatusDatum()
	{
		return statusDatum;
	}

	public void setStatusDatum(Date statusDatum)
	{
		this.statusDatum = statusDatum;
	}

	public boolean isPopulatie()
	{
		return populatie;
	}

	public void setPopulatie(boolean populatie)
	{
		this.populatie = populatie;
	}

	public boolean isAttributen()
	{
		return attributen;
	}

	public void setAttributen(boolean attributen)
	{
		this.attributen = attributen;
	}

	public Date getUploadDatum()
	{
		return uploadDatum;
	}

	public void setUploadDatum(Date uploadDatum)
	{
		this.uploadDatum = uploadDatum;
	}

	public String getToepassenOp()
	{
		return toepassenOp;
	}

	public void setToepassenOp(String toepassenOp)
	{
		this.toepassenOp = toepassenOp;
	}

	public ProjectBestandVerwerking getVerwerking()
	{
		return verwerking;
	}

	public void setVerwerking(ProjectBestandVerwerking verwerking)
	{
		this.verwerking = verwerking;
	}

	public ProjectBestandType getType()
	{
		return type;
	}

	public void setType(ProjectBestandType type)
	{
		this.type = type;
	}

	public String getDynamischeInactiveerReden()
	{
		return dynamischeInactiveerReden;
	}

	public void setDynamischeInactiveerReden(String dynamischeInactiveerReden)
	{
		this.dynamischeInactiveerReden = dynamischeInactiveerReden;
	}
}
