package nl.rivm.screenit.model.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.UniqueConstraint;

import lombok.Data;
import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.cervix.enums.CervixLeeftijdcategorie;
import nl.rivm.screenit.model.cervix.verslag.CervixVerslag;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Getter
@Setter
@Entity
@Table(
	schema = "cervix",
	name = "screening_ronde",
	indexes = { @Index(name = "idx_CERVIX_SCREENING_RONDE_STATUS", columnList = "status"),
		@Index(name = "idx_CERVIX_SCREENING_RONDE_LEEFTIJDCATEGORIE", columnList = "leeftijdcategorie"),
		@Index(name = "idx_CERVIX_SCREENING_RONDE_IN_VERVOLGONDERZOEK_DATUM", columnList = "inVervolgonderzoekDatum") },
	uniqueConstraints = {
		@UniqueConstraint(columnNames = "laatste_afmelding"), @UniqueConstraint(columnNames = "laatste_brief"), @UniqueConstraint(columnNames = "laatste_uitnodiging"),
		@UniqueConstraint(columnNames = "uitnodiging_vervolgonderzoek"), @UniqueConstraint(columnNames = "uitstel"), @UniqueConstraint(columnNames = "monster_hpv_uitslag"),
		@UniqueConstraint(columnNames = "uitstrijkje_cytologie_uitslag"), @UniqueConstraint(columnNames = "uitstrijkje_vervolgonderzoek_uitslag") })
@Audited
public class CervixScreeningRonde extends ScreeningRonde<CervixDossier, CervixBrief, CervixAfmelding, CervixUitnodiging>
{

	private static final long serialVersionUID = 1L;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private CervixDossier dossier;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private CervixLeeftijdcategorie leeftijdcategorie;

	@OneToMany(mappedBy = "screeningRonde", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<CervixUitnodiging> uitnodigingen = new ArrayList<>();

	@OneToOne(fetch = FetchType.LAZY)
	private CervixUitnodiging eersteUitnodiging;

	@OneToOne(fetch = FetchType.LAZY)
	private CervixUitnodiging laatsteUitnodiging;

	@OneToOne(fetch = FetchType.LAZY)
	private CervixUitnodiging uitnodigingVervolgonderzoek;

	@OneToMany(mappedBy = "screeningRonde", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<CervixBrief> brieven = new ArrayList<>();

	@OneToOne(fetch = FetchType.LAZY)
	private CervixBrief laatsteBrief;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "screeningRonde")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	@NotAudited
	private List<CervixHuisartsBericht> huisartsBerichten = new ArrayList<>();

	@OneToMany(mappedBy = "screeningRonde", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<CervixAfmelding> afmeldingen = new ArrayList<>();

	@OneToOne(fetch = FetchType.LAZY)
	private CervixAfmelding laatsteAfmelding;

	@OneToOne(fetch = FetchType.LAZY)
	private CervixUitstel uitstel;

	@OneToOne(fetch = FetchType.EAGER)
	private CervixMonster monsterHpvUitslag;

	@OneToOne(fetch = FetchType.EAGER)
	private CervixUitstrijkje uitstrijkjeCytologieUitslag;

	@Temporal(TemporalType.TIMESTAMP)
	private Date inVervolgonderzoekDatum;

	@OneToOne(fetch = FetchType.EAGER)
	private CervixUitstrijkje uitstrijkjeVervolgonderzoekUitslag;

	@OneToOne(fetch = FetchType.LAZY)
	private CervixUitnodiging laatsteZasUitnodiging;

	@OneToMany(mappedBy = "screeningRonde", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<CervixVerslag> verslagen = new ArrayList<>();

	@Column(nullable = true)
	private LocalDate controleUitstrijkjeDatum;

}
