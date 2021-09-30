package nl.rivm.screenit.model.mamma;

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

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.enums.MammaLezingRedenenFotobesprekingMbber;
import nl.rivm.screenit.model.mamma.enums.MammaLezingRedenenFotobesprekingRadioloog;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Getter
@Setter
@Audited
@Table(
	schema = "mamma",
	name = "conclusie_review",
	uniqueConstraints = @UniqueConstraint(columnNames = { "screening_ronde", "radioloog" }))
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
public class MammaConclusieReview extends AbstractHibernateObject
{
	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private MammaScreeningRonde screeningRonde;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private InstellingGebruiker radioloog;

	@Column(nullable = false)
	private LocalDateTime reviewMoment;

	@ElementCollection(targetClass = MammaLezingRedenenFotobesprekingRadioloog.class)
	@Enumerated(EnumType.STRING)
	@Column(nullable = true)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
	@CollectionTable(schema = "mamma", name = "conclusie_review_redenen_fotobespreking_radioloog")
	private List<MammaLezingRedenenFotobesprekingRadioloog> redenenFotobesprekingRadioloog = new ArrayList<>();

	@ElementCollection(targetClass = MammaLezingRedenenFotobesprekingMbber.class)
	@Enumerated(EnumType.STRING)
	@Column(nullable = true)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
	@CollectionTable(schema = "mamma", name = "conclusie_review_redenen_fotobespreking_mbber")
	private List<MammaLezingRedenenFotobesprekingMbber> redenenFotobesprekingMbber = new ArrayList<>();

}
