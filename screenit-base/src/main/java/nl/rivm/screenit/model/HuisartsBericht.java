package nl.rivm.screenit.model;

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
import javax.persistence.ManyToOne;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.enums.HuisartsBerichtType;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Setter
@Getter
@Entity
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public abstract class HuisartsBericht extends TablePerClassHibernateObject
{
	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private Client client;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private ScreeningOrganisatie screeningsOrganisatie;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private HuisartsBerichtType berichtType;

	@Column(length = 7000)
	private String berichtInhoud;

	private Date aanmaakDatum;

	@Column(nullable = false)
	private boolean isEenOpnieuwVerzondenBericht;

}
