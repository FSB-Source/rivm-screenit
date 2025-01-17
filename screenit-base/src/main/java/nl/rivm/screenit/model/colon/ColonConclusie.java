package nl.rivm.screenit.model.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.colon.enums.ColonConclusieOnHoldReden;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.enums.ColonGeenOnderzoekReden;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "colon", indexes = { @Index(name = "idx_colon_conclusie_type", columnList = "type") })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
@Getter
@Setter
public class ColonConclusie extends AbstractHibernateObject
{
	private static final long serialVersionUID = 1L;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private ColonConclusieType type;

	@Enumerated(EnumType.STRING)
	private ColonConclusieOnHoldReden onHoldReden;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private InstellingGebruiker instellingGebruiker;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = false)
	private Date datum;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = true)
	private Date datumColoscopie;

	@Column(nullable = true)
	private Boolean coloscopieDatumOpVerzoekClient;

	@Column(nullable = true)
	private Integer asaScore;

	@Enumerated(EnumType.STRING)
	@Column(nullable = true)
	private ColonGeenOnderzoekReden geenOnderzoekReden;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = true)
	private Date noShowBericht;

	private Boolean doorverwijzingBevestigd;
}
