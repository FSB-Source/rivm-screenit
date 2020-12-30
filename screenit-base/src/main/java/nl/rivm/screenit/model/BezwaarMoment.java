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
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.Transient;

import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(schema = "algemeen")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public class BezwaarMoment extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private Client client;

	@ManyToOne(fetch = FetchType.LAZY, optional = true, cascade = CascadeType.ALL)
	private BezwaarBrief bezwaarAanvraag;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private AanvraagBriefStatus status;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date statusDatum;

	@OneToOne(fetch = FetchType.LAZY, optional = true, cascade = CascadeType.ALL)
	@NotAudited
	private UploadDocument bezwaarBrief;

	private Date bezwaarDatum;

	@OneToOne(fetch = FetchType.LAZY, optional = true, cascade = CascadeType.ALL)
	private BezwaarBrief bevestigingsbrief;

	@OneToMany(cascade = CascadeType.ALL, mappedBy = "bezwaarMoment")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<BezwaarBrief> brieven = new ArrayList<BezwaarBrief>();

	@NotAudited
	@OneToMany(cascade = CascadeType.ALL, mappedBy = "bezwaarMoment", fetch = FetchType.LAZY)
	private List<Bezwaar> bezwaren = new ArrayList<Bezwaar>();

	@Transient
	private ClientContactManier manier;

	public Date getBezwaarDatum()
	{
		return bezwaarDatum;
	}

	public void setBezwaarDatum(Date bezwaarDatum)
	{
		this.bezwaarDatum = bezwaarDatum;
	}

	public Client getClient()
	{
		return client;
	}

	public void setClient(Client client)
	{
		this.client = client;
	}

	public UploadDocument getBezwaarBrief()
	{
		return bezwaarBrief;
	}

	public void setBezwaarBrief(UploadDocument bezwaarBrief)
	{
		this.bezwaarBrief = bezwaarBrief;
	}

	public AanvraagBriefStatus getStatus()
	{
		return status;
	}

	public void setStatus(AanvraagBriefStatus status)
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

	public BezwaarBrief getBezwaarAanvraag()
	{
		return bezwaarAanvraag;
	}

	public void setBezwaarAanvraag(BezwaarBrief bezwaarAanvraag)
	{
		this.bezwaarAanvraag = bezwaarAanvraag;
	}

	public List<BezwaarBrief> getBrieven()
	{
		return brieven;
	}

	public void setBrieven(List<BezwaarBrief> brieven)
	{
		this.brieven = brieven;
	}

	public BezwaarBrief getBevestigingsbrief()
	{
		return bevestigingsbrief;
	}

	public void setBevestigingsbrief(BezwaarBrief bevestigingsbrief)
	{
		this.bevestigingsbrief = bevestigingsbrief;
	}

	public List<Bezwaar> getBezwaren()
	{
		return bezwaren;
	}

	public void setBezwaren(List<Bezwaar> bezwaren)
	{
		this.bezwaren = bezwaren;
	}

	public ClientContactManier getManier()
	{
		return manier;
	}

	public void setManier(ClientContactManier manier)
	{
		this.manier = manier;
	}
}
