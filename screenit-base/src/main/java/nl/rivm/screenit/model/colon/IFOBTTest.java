package nl.rivm.screenit.model.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Index;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.enums.RedenNietTeBeoordelen;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "colon", indexes = { @Index(name = "IFOBTEST_STATUS", columnList = "status"), @Index(name = "idx_ifobttest_barcode", columnList = "barcode", unique = true) })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public class IFOBTTest extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@Enumerated(EnumType.STRING)
	private IFOBTTestStatus status;

	@Enumerated(EnumType.STRING)
	private RedenNietTeBeoordelen redenNietTeBeoordelen;

	private boolean herinnering;

	@Temporal(TemporalType.TIMESTAMP)
	private Date statusDatum;

	@Temporal(TemporalType.TIMESTAMP)
	private Date verwerkingsDatum;

	@Temporal(TemporalType.TIMESTAMP)
	private Date datumVerstuurd;

	@ManyToOne(optional = false)
	private ColonScreeningRonde colonScreeningRonde;

	@Column(nullable = false, unique = true)
	private String barcode;

	@OneToOne(mappedBy = "gekoppeldeTest", optional = true)
	private ColonUitnodiging colonUitnodiging;

	@OneToOne(mappedBy = "gekoppeldeExtraTest", optional = true)
	private ColonUitnodiging colonUitnodigingExtra;

	@Deprecated
	@Temporal(TemporalType.DATE)
	private Date afnameDatum;

	@Temporal(TemporalType.DATE)
	private Date analyseDatum;

	private BigDecimal uitslag;

	private BigDecimal normWaarde;

	@Enumerated(EnumType.STRING)
	private ColonGeinterpreteerdeUitslag geinterpreteerdeUitslag;

	@ManyToOne
	@JoinColumn(name = "ifobtLaboratorium_id")
	private IFobtLaboratorium ifobtLaboratorium;

	private String instumentId;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private IFOBTType type;

	@ManyToOne
	private UploadDocument verwijderbrief;

	@Enumerated(EnumType.STRING)
	private PreferenceKey heraanmeldenTekstKey;

	public IFOBTTestStatus getStatus()
	{
		return status;
	}

	public void setStatus(IFOBTTestStatus status)
	{
		this.status = status;
	}

	public Date getStatusDatum()
	{
		return statusDatum;
	}

	public void setStatusDatum(Date statusDatum)
	{
		this.statusDatum = statusDatum;
	}

	public ColonScreeningRonde getColonScreeningRonde()
	{
		return colonScreeningRonde;
	}

	public void setColonScreeningRonde(ColonScreeningRonde colonScreeningRonde)
	{
		this.colonScreeningRonde = colonScreeningRonde;
	}

	public String getBarcode()
	{
		return barcode;
	}

	public void setBarcode(String barcode)
	{
		this.barcode = barcode;
	}

	public Date getDatumVerstuurd()
	{
		return datumVerstuurd;
	}

	public void setDatumVerstuurd(Date datumVerstuurd)
	{
		this.datumVerstuurd = datumVerstuurd;
	}

	public ColonUitnodiging getColonUitnodiging()
	{
		return colonUitnodiging;
	}

	public void setColonUitnodiging(ColonUitnodiging colonUitnodiging)
	{
		this.colonUitnodiging = colonUitnodiging;
	}

	public ColonUitnodiging getColonUitnodigingExtra()
	{
		return colonUitnodigingExtra;
	}

	public void setColonUitnodigingExtra(ColonUitnodiging colonUitnodigingExtra)
	{
		this.colonUitnodigingExtra = colonUitnodigingExtra;
	}

	@Deprecated
	public Date getAfnameDatum()
	{
		return afnameDatum;
	}

	@Deprecated
	public void setAfnameDatum(Date afnameDatum)
	{
		this.afnameDatum = afnameDatum;
	}

	public Date getAnalyseDatum()
	{
		return analyseDatum;
	}

	public void setAnalyseDatum(Date analyseDatum)
	{
		this.analyseDatum = analyseDatum;
	}

	public BigDecimal getUitslag()
	{
		return uitslag;
	}

	public void setUitslag(BigDecimal uitslag)
	{
		this.uitslag = uitslag;
	}

	public RedenNietTeBeoordelen getRedenNietTeBeoordelen()
	{
		return redenNietTeBeoordelen;
	}

	public void setRedenNietTeBeoordelen(RedenNietTeBeoordelen redenNietTeBeoordelen)
	{
		this.redenNietTeBeoordelen = redenNietTeBeoordelen;
	}

	public BigDecimal getNormWaarde()
	{
		return normWaarde;
	}

	public void setNormWaarde(BigDecimal normWaarde)
	{
		this.normWaarde = normWaarde;
	}

	public ColonGeinterpreteerdeUitslag getGeinterpreteerdeUitslag()
	{
		return geinterpreteerdeUitslag;
	}

	public void setGeinterpreteerdeUitslag(ColonGeinterpreteerdeUitslag geinterpreteerdeUitslag)
	{
		this.geinterpreteerdeUitslag = geinterpreteerdeUitslag;
	}

	public IFobtLaboratorium getIfobtLaboratorium()
	{
		return ifobtLaboratorium;
	}

	public void setIfobtLaboratorium(IFobtLaboratorium ifobtLaboratorium)
	{
		this.ifobtLaboratorium = ifobtLaboratorium;
	}

	public String getInstumentId()
	{
		return instumentId;
	}

	public void setInstumentId(String instumentId)
	{
		this.instumentId = instumentId;
	}

	public boolean isHerinnering()
	{
		return herinnering;
	}

	public void setHerinnering(boolean herinnering)
	{
		this.herinnering = herinnering;
	}

	public Date getVerwerkingsDatum()
	{
		return verwerkingsDatum;
	}

	public void setVerwerkingsDatum(Date verwerkingsDatum)
	{
		this.verwerkingsDatum = verwerkingsDatum;
	}

	public IFOBTType getType()
	{
		return type;
	}

	public void setType(IFOBTType type)
	{
		this.type = type;
	}

	public UploadDocument getVerwijderbrief()
	{
		return verwijderbrief;
	}

	public void setVerwijderbrief(UploadDocument verwijderbrief)
	{
		this.verwijderbrief = verwijderbrief;
	}

	public PreferenceKey getHeraanmeldenTekstKey()
	{
		return heraanmeldenTekstKey;
	}

	public void setHeraanmeldenTekstKey(PreferenceKey heraanmeldenTekstKey)
	{
		this.heraanmeldenTekstKey = heraanmeldenTekstKey;
	}
}
