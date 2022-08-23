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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
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

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.SingleTableHibernateObject;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.cervix.enums.CervixNietAnalyseerbaarReden;
import nl.rivm.screenit.model.cervix.enums.signaleringen.CervixMonsterSignalering;
import nl.rivm.screenit.model.cervix.facturatie.CervixVerrichting;
import nl.rivm.screenit.util.SkipFieldForDiff;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Setter
@Getter
@Entity
@Table(
	schema = "cervix",
	name = "monster",
	uniqueConstraints = {
		@UniqueConstraint(columnNames = "brief"),
		@UniqueConstraint(columnNames = "laatste_hpv_beoordeling"),
		@UniqueConstraint(columnNames = "labformulier"),
		@UniqueConstraint(columnNames = "cytologie_order"),
		@UniqueConstraint(columnNames = "cytologie_verslag"),
		@UniqueConstraint(columnNames = "huisarts_bericht") },
	indexes = {
		@Index(name = "idx_CERVIX_MONSTER_STATUS_DATUM", columnList = "statusDatum"),
		@Index(name = "idx_CERVIX_MONSTER_ONTVANGSTDATUM", columnList = "ontvangstdatum"),
		@Index(name = "idx_CERVIX_MONSTER_DATUM_ORU_VERSTUURD", columnList = "datumOruVerstuurd"),
		@Index(name = "idx_CERVIX_ZAS_ZAS_STATUS", columnList = "zasStatus") })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public abstract class CervixMonster extends SingleTableHibernateObject
{
	@OneToMany(mappedBy = "monster", fetch = FetchType.LAZY)
	private List<CervixVerrichting> verrichtingen;

	private static final long serialVersionUID = 1L;

	@OneToOne(mappedBy = "monster", optional = false, fetch = FetchType.LAZY)
	private CervixUitnodiging uitnodiging;

	@Column(unique = true)
	private String monsterId;

	@Temporal(TemporalType.TIMESTAMP)
	private Date ontvangstdatum;

	@ManyToOne(optional = true, fetch = FetchType.EAGER)
	private CervixScreeningRonde ontvangstScreeningRonde;

	@ManyToOne(fetch = FetchType.LAZY)
	private BMHKLaboratorium laboratorium;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date statusDatum;

	@Enumerated(EnumType.STRING)
	private CervixNietAnalyseerbaarReden nietAnalyseerbaarReden;

	@ElementCollection(targetClass = CervixMonsterSignalering.class)
	@Column
	@Enumerated(EnumType.STRING)
	@CollectionTable(schema = "cervix", name = "monster_signalering")
	@SkipFieldForDiff
	private List<CervixMonsterSignalering> signaleringen = new ArrayList<>();

	@Column
	@Temporal(TemporalType.TIMESTAMP)
	private Date datumOruVerstuurd;

	@NotAudited
	@OneToMany(fetch = FetchType.LAZY, mappedBy = "monster")
	private List<CervixHpvBeoordeling> hpvBeoordelingen = new ArrayList<>();

	@NotAudited
	@OneToOne(fetch = FetchType.EAGER)
	private CervixHpvBeoordeling laatsteHpvBeoordeling;

	@OneToOne(fetch = FetchType.LAZY)
	private CervixBrief brief;

	@Temporal(TemporalType.TIMESTAMP)
	private Date verwijderdDatum;

	@ManyToOne(fetch = FetchType.LAZY)
	private UploadDocument verwijderdBrief;

	@ElementCollection(fetch = FetchType.LAZY)
	@CollectionTable(schema = "cervix", name = "monster_barcode_afgedrukt")
	@NotAudited
	private List<Date> barcodeAfgedrukt = new ArrayList<>();

	@Column
	private String overigeSignalering;

}
