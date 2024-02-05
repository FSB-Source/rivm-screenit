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
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.HuisartsBericht;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsBerichtStatus;
import nl.rivm.screenit.model.cervix.enums.CervixOmissieType;

import org.hibernate.envers.Audited;

@Setter
@Getter
@Entity
@Table(schema = "cervix", name = "huisarts_bericht", indexes = { @Index(name = "idx_CERVIX_HUISARTS_BERICHT_STATUS", columnList = "status") })
@Audited
public class CervixHuisartsBericht extends HuisartsBericht
{

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private CervixScreeningRonde screeningRonde;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private CervixHuisartsBerichtStatus status;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date statusDatum;

	@OneToOne(mappedBy = "huisartsBericht", fetch = FetchType.LAZY)
	private CervixUitstrijkje uitstrijkje;

	@OneToOne(mappedBy = "uitstrijkjeOntbreektHuisartsBericht", fetch = FetchType.LAZY)
	private CervixLabformulier labformulier;

	@ManyToOne(fetch = FetchType.EAGER)
	private CervixHuisartsLocatie huisartsLocatie;

	@Enumerated(EnumType.STRING)
	private CervixOmissieType omissieType;

	@ManyToOne(fetch = FetchType.LAZY)
	private CervixHuisartsLocatie extraHuisartsLocatie;

	@Temporal(TemporalType.TIMESTAMP)
	private Date extraHuisartsLocatieVerstuurdDatum;

}
