package nl.rivm.screenit.model.cervix;

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
import javax.persistence.OneToOne;
import javax.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.cervix.enums.CervixHpvBeoordelingWaarde;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;

@Entity
@Table(schema = "cervix", name = "hpv_beoordeling", indexes = { @Index(name = "idx_CERVIX_HPV_BEOORDELING_HPV_UITSLAG", columnList = "hpvUitslag") })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Getter
@Setter
public class CervixHpvBeoordeling extends AbstractHibernateObject
{
	@Column(nullable = false)
	private Date analyseDatum;

	@Column(nullable = false)
	private Date autorisatieDatum;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private CervixHpvBeoordelingWaarde hpvUitslag;

	@OneToOne(fetch = FetchType.LAZY, optional = true, cascade = javax.persistence.CascadeType.REMOVE)
	@Cascade(CascadeType.DELETE)
	private CervixHpvAnalyseresultaten analyseresultaten;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private CervixHpvBericht hpvBericht;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private CervixMonster monster;

}
