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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.Index;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.UniqueConstraint;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DeelnamemodusDossier;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.cervix.cis.CervixCISHistorie;
import nl.rivm.screenit.model.enums.Deelnamemodus;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(
	schema = "cervix",
	name = "dossier",
	indexes = { @Index(name = "idx_CERVIX_DOSSIER_VOLGENDE_RONDE_VANAF", columnList = "volgendeRondeVanaf"), @Index(name = "idx_CERVIX_DOSSIER_STATUS", columnList = "status") },
	uniqueConstraints = { @UniqueConstraint(columnNames = "laatste_screening_ronde"), @UniqueConstraint(columnNames = "laatste_afmelding") })
@Audited
@Getter
@Setter
public class CervixDossier extends Dossier<CervixScreeningRonde, CervixAfmelding> implements DeelnamemodusDossier
{
	@OneToOne(mappedBy = "cervixDossier", optional = false)
	private Client client;

	@OneToOne(fetch = FetchType.LAZY)
	@NotAudited
	private CervixCISHistorie cisHistorie;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "dossier")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<CervixScreeningRonde> screeningRondes = new ArrayList<>();

	@OneToOne(optional = true, fetch = FetchType.EAGER)
	private CervixScreeningRonde laatsteScreeningRonde;

	@OneToMany(mappedBy = "dossier", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<CervixAfmelding> afmeldingen = new ArrayList<>();

	@OneToOne(optional = true, fetch = FetchType.EAGER)
	private CervixAfmelding laatsteAfmelding;

	@Temporal(TemporalType.DATE)
	private Date volgendeRondeVanaf;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private Deelnamemodus deelnamemodus = Deelnamemodus.STANDAARD;

	@OneToOne(optional = true, fetch = FetchType.EAGER)
	private CervixBrief vooraankondigingsBrief;

}
