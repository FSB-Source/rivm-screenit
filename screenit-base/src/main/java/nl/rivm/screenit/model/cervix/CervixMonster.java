package nl.rivm.screenit.model.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

	public UploadDocument getVerwijderdBrief()
	{
		return verwijderdBrief;
	}

	public void setVerwijderdBrief(UploadDocument verwijderdBrief)
	{
		this.verwijderdBrief = verwijderdBrief;
	}

	public CervixUitnodiging getUitnodiging()
	{
		return uitnodiging;
	}

	public void setUitnodiging(CervixUitnodiging uitnodiging)
	{
		this.uitnodiging = uitnodiging;
	}

	public BMHKLaboratorium getLaboratorium()
	{
		return laboratorium;
	}

	public void setLaboratorium(BMHKLaboratorium laboratorium)
	{
		this.laboratorium = laboratorium;
	}

	public String getMonsterId()
	{
		return monsterId;
	}

	public void setMonsterId(String monsterId)
	{
		this.monsterId = monsterId;
	}

	public Date getOntvangstdatum()
	{
		return ontvangstdatum;
	}

	public void setOntvangstdatum(Date ontvangstdatum)
	{
		this.ontvangstdatum = ontvangstdatum;
	}

	public CervixBrief getBrief()
	{
		return brief;
	}

	public void setBrief(CervixBrief brief)
	{
		this.brief = brief;
	}

	public List<CervixHpvBeoordeling> getHpvBeoordelingen()
	{
		return hpvBeoordelingen;
	}

	public void setHpvBeoordelingen(List<CervixHpvBeoordeling> hpvBeoordelingen)
	{
		this.hpvBeoordelingen = hpvBeoordelingen;
	}

	public CervixHpvBeoordeling getLaatsteHpvBeoordeling()
	{
		return laatsteHpvBeoordeling;
	}

	public void setLaatsteHpvBeoordeling(CervixHpvBeoordeling laatsteHpvBeoordeling)
	{
		this.laatsteHpvBeoordeling = laatsteHpvBeoordeling;
	}

	public Date getStatusDatum()
	{
		return statusDatum;
	}

	public void setStatusDatum(Date statusDatum)
	{
		this.statusDatum = statusDatum;
	}

	public List<CervixVerrichting> getVerrichtingen()
	{
		return verrichtingen;
	}

	public void setVerrichtingen(List<CervixVerrichting> verrichtingen)
	{
		this.verrichtingen = verrichtingen;
	}

	public Date getVerwijderdDatum()
	{
		return verwijderdDatum;
	}

	public void setVerwijderdDatum(Date verwijderdDatum)
	{
		this.verwijderdDatum = verwijderdDatum;
	}

	public Date getDatumOruVerstuurd()
	{
		return datumOruVerstuurd;
	}

	public void setDatumOruVerstuurd(Date datumOruVerstuurd)
	{
		this.datumOruVerstuurd = datumOruVerstuurd;
	}

	public CervixScreeningRonde getOntvangstScreeningRonde()
	{
		return ontvangstScreeningRonde;
	}

	public void setOntvangstScreeningRonde(CervixScreeningRonde ontvangstScreeningRonde)
	{
		this.ontvangstScreeningRonde = ontvangstScreeningRonde;
	}

	public CervixNietAnalyseerbaarReden getNietAnalyseerbaarReden()
	{
		return nietAnalyseerbaarReden;
	}

	public void setNietAnalyseerbaarReden(CervixNietAnalyseerbaarReden nietAnalyseerbaarReden)
	{
		this.nietAnalyseerbaarReden = nietAnalyseerbaarReden;
	}

	public List<CervixMonsterSignalering> getSignaleringen()
	{
		return signaleringen;
	}

	public void setSignaleringen(List<CervixMonsterSignalering> signaleringen)
	{
		this.signaleringen = signaleringen;
	}

	public List<Date> getBarcodeAfgedrukt()
	{
		return barcodeAfgedrukt;
	}

	public void setBarcodeAfgedrukt(List<Date> barcodeAfgedrukt)
	{
		this.barcodeAfgedrukt = barcodeAfgedrukt;
	}

	public String getOverigeSignalering()
	{
		return overigeSignalering;
	}

	public void setOverigeSignalering(String overigeSignalering)
	{
		this.overigeSignalering = overigeSignalering;
	}
}
