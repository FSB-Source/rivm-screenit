package nl.rivm.screenit.model.cervix;

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
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierAanvraagStatus;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "cervix", name = "labformulier_aanvraag", uniqueConstraints = { @UniqueConstraint(columnNames = "brief"), @UniqueConstraint(columnNames = "voorblad_brief") })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public class CervixLabformulierAanvraag extends AbstractHibernateObject implements ICervixHuisartsportaalObject
{

	private static final long serialVersionUID = 1L;

	private Long huisartsportaalId;

	@Column(nullable = false)
	private Integer aantal;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private CervixLabformulierAanvraagStatus status;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date statusDatum;

	@OneToOne(optional = false, fetch = FetchType.LAZY)
	private CervixRegioBrief brief;

	@OneToOne(optional = false, fetch = FetchType.LAZY)
	private CervixRegioBrief voorbladBrief;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date aanvraagDatum;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private CervixHuisartsLocatie huisartsLocatie;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private InstellingGebruiker instellingGebruiker;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date mutatiedatum;

	public CervixLabformulierAanvraagStatus getStatus()
	{
		return status;
	}

	public void setStatus(CervixLabformulierAanvraagStatus status)
	{
		this.status = status;
	}

	public Date getAanvraagDatum()
	{
		return aanvraagDatum;
	}

	public void setAanvraagDatum(Date aanvraagDatum)
	{
		this.aanvraagDatum = aanvraagDatum;
	}

	public InstellingGebruiker getInstellingGebruiker()
	{
		return instellingGebruiker;
	}

	public void setInstellingGebruiker(InstellingGebruiker instellingGebruiker)
	{
		this.instellingGebruiker = instellingGebruiker;
	}

	public Date getStatusDatum()
	{
		return statusDatum;
	}

	public void setStatusDatum(Date statusDatum)
	{
		this.statusDatum = statusDatum;
	}

	public CervixRegioBrief getBrief()
	{
		return brief;
	}

	public void setBrief(CervixRegioBrief brief)
	{
		this.brief = brief;
	}

	public CervixRegioBrief getVoorbladBrief()
	{
		return voorbladBrief;
	}

	public void setVoorbladBrief(CervixRegioBrief voorbladBrief)
	{
		this.voorbladBrief = voorbladBrief;
	}

	public void setAantal(Integer aantal)
	{
		this.aantal = aantal;
	}

	public CervixHuisartsLocatie getHuisartsLocatie()
	{
		return huisartsLocatie;
	}

	public void setHuisartsLocatie(CervixHuisartsLocatie locatie)
	{
		this.huisartsLocatie = locatie;
	}

	public Integer getAantal()
	{
		return aantal;
	}

	@Override
	public Long getHuisartsportaalId()
	{
		return huisartsportaalId;
	}

	@Override
	public void setHuisartsportaalId(Long huisartsportaalId)
	{
		this.huisartsportaalId = huisartsportaalId;
	}

	@Override
	public void setScreenitId(Long id)
	{
		setId(id);
	}

	@Override
	public Long getScreenitId()
	{
		return getId();
	}

	@Override
	public Date getMutatiedatum()
	{
		return mutatiedatum;
	}

	@Override
	public void setMutatiedatum(Date mutatiedatum)
	{
		this.mutatiedatum = mutatiedatum;
	}
}
