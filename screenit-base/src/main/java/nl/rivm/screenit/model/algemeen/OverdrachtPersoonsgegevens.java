package nl.rivm.screenit.model.algemeen;

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

import java.util.Date;

import javax.persistence.CascadeType;
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

import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.UploadDocument;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "algemeen")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public class OverdrachtPersoonsgegevens extends AbstractHibernateObject
{
	private static final long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private Client client;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private AanvraagBriefStatus status;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date statusDatum;

	@Column(nullable = false)
	private Boolean bkGegevens;

	@Column(nullable = false)
	private Boolean bkBeelden;

	@Column(nullable = false)
	private Boolean bmhkGegevens;

	@Column(nullable = false)
	private Boolean dkGegevens;

	@OneToOne(fetch = FetchType.LAZY, optional = false, cascade = CascadeType.ALL)
	private AlgemeneBrief verstuurdeAanvraagbrief;

	@OneToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
	private UploadDocument ontvangenAanvraagbrief;

	@OneToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
	private AlgemeneBrief geenHandtekeningBrief;

	public Client getClient()
	{
		return client;
	}

	public void setClient(Client client)
	{
		this.client = client;
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

	public AlgemeneBrief getVerstuurdeAanvraagbrief()
	{
		return verstuurdeAanvraagbrief;
	}

	public void setVerstuurdeAanvraagbrief(AlgemeneBrief verstuurdeAanvraagBrief)
	{
		this.verstuurdeAanvraagbrief = verstuurdeAanvraagBrief;
	}

	public UploadDocument getOntvangenAanvraagbrief()
	{
		return ontvangenAanvraagbrief;
	}

	public void setOntvangenAanvraagbrief(UploadDocument ontvangenAanvraagBrief)
	{
		this.ontvangenAanvraagbrief = ontvangenAanvraagBrief;
	}

	public AlgemeneBrief getGeenHandtekeningBrief()
	{
		return geenHandtekeningBrief;
	}

	public void setGeenHandtekeningBrief(AlgemeneBrief geenHandtekeningBrief)
	{
		this.geenHandtekeningBrief = geenHandtekeningBrief;
	}

	public Boolean getBkGegevens()
	{
		return bkGegevens;
	}

	public void setBkGegevens(Boolean bkGegevens)
	{
		this.bkGegevens = bkGegevens;
	}

	public Boolean getBkBeelden()
	{
		return bkBeelden;
	}

	public void setBkBeelden(Boolean bkBeelden)
	{
		this.bkBeelden = bkBeelden;
	}

	public Boolean getBmhkGegevens()
	{
		return bmhkGegevens;
	}

	public void setBmhkGegevens(Boolean bmhkGegevens)
	{
		this.bmhkGegevens = bmhkGegevens;
	}

	public Boolean getDkGegevens()
	{
		return dkGegevens;
	}

	public void setDkGegevens(Boolean dkGegevens)
	{
		this.dkGegevens = dkGegevens;
	}
}
