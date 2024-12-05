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

import lombok.Getter;
import lombok.Setter;

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
@Getter
@Setter
public class CervixLabformulierAanvraag extends AbstractHibernateObject implements ICervixHuisartsportaalObject
{
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
