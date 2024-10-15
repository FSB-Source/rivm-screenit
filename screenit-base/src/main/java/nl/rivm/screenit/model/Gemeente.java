
package nl.rivm.screenit.model;

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
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.colon.UitnodigingsGebied;
import nl.rivm.screenit.model.gba.GbaStamtabel;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(schema = "algemeen", indexes = @Index(columnList = "code", name = "IDX_GEMEENTE_CODE", unique = true))
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
@Getter
@Setter
public class Gemeente extends AbstractHibernateObject implements GbaStamtabel, IGeografischeCoordinaten
{
	public static final String RNI_CODE = "1999";

	@Column(unique = true, nullable = false)
	private String code;

	private String naam;

	@Temporal(TemporalType.DATE)
	private Date beginDatum;

	@Temporal(TemporalType.DATE)
	private Date eindDatum;

	@OneToMany(mappedBy = "gemeente", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	@NotAudited
	private List<UitnodigingsGebied> uitnodigingsGebieden = new ArrayList<>();

	@ManyToOne(fetch = FetchType.EAGER)
	private ScreeningOrganisatie screeningOrganisatie;

	@ManyToOne(fetch = FetchType.LAZY)
	private BMHKLaboratorium bmhkLaboratorium;

	@ManyToOne(optional = true, fetch = FetchType.EAGER)
	private Gemeente opvolgGemeente;

	@Column(precision = HibernateMagicNumber.P9, scale = HibernateMagicNumber.S6)
	private BigDecimal latitude;

	@Column(precision = HibernateMagicNumber.P9, scale = HibernateMagicNumber.S6)
	private BigDecimal longitude;
}
