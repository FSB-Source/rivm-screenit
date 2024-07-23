package nl.rivm.screenit.model.mamma;

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

import java.math.BigDecimal;
import java.time.LocalDateTime;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "mamma", name = "afspraakReservering")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
@Getter
@Setter
public class MammaAfspraakReservering extends AbstractHibernateObject
{
	@OneToOne(optional = false, fetch = FetchType.LAZY)
	private Client client;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private InstellingGebruiker medewerker;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private MammaCapaciteitBlok capaciteitBlok;

	@Column(nullable = false, precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S5)
	private BigDecimal opkomstkans;

	@Column(nullable = false)
	private LocalDateTime vanaf;

	@Column(nullable = false)
	private LocalDateTime aangemaaktOp;

}
