
package nl.rivm.screenit.model.colon;

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
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.Transient;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.Afspraak;
import nl.rivm.screenit.model.colon.enums.ColonTijdSlotType;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;

import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;

@Getter
@Setter
@Entity
@Table(schema = "colon", indexes = { @Index(name = "idx_colon_afspraak_bezwaar", columnList = "bezwaar") })
@Audited
public class ColonIntakeAfspraak extends Afspraak
{
	private static final long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY)
	@Cascade({ CascadeType.SAVE_UPDATE })
	private ColonScreeningRonde colonScreeningRonde;

	@OneToOne(fetch = FetchType.LAZY)
	private ColonConclusie conclusie;

	@Temporal(TemporalType.TIMESTAMP)
	private Date afzegDatum;

	@Column(precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S3, nullable = false)
	private BigDecimal afstand;

	@Column(nullable = false)
	private Boolean bezwaar;

	@Transient
	private Boolean briefTegenhouden;

	public ColonIntakeAfspraak()
	{
		super();
		setTitle(ColonTijdSlotType.AFSPRAAK.getTitle());
	}
}
