package nl.rivm.screenit.huisartsenportaal.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import org.hibernate.envers.Audited;

@Entity
@Audited
@Table(name = "betaling", indexes = { @Index(name = "idx_BETALING_BETALINGS_DATUM", columnList = "betalingsdatum") })
@Getter
@Setter
public class Betaling extends AbstractReferenceObject
{

	@Column(nullable = false)
	private BigDecimal bedrag;

	@Column(nullable = false)
	private boolean debet;

	private String betalingsKenmerk;

	@Temporal(TemporalType.TIMESTAMP)
	private Date betalingsdatum;

	@ManyToOne(fetch = FetchType.LAZY)
	private Verrichting verrichting;

}
