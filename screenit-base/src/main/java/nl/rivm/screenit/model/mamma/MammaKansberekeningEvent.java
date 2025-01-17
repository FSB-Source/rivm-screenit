package nl.rivm.screenit.model.mamma;

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

import java.math.BigDecimal;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.TablePerClassHibernateObject;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;

import org.hibernate.envers.Audited;

@Entity
@Audited
public abstract class MammaKansberekeningEvent extends TablePerClassHibernateObject
{
	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date wijzigingsDatum;

	@Column(nullable = false)
	private Long peilEpochDay;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaDoelgroep doelgroep;

	@Column(nullable = false)
	private Boolean tehuis;

	@Column(nullable = false)
	private Integer voorgaandeScreeningRondes = 0;

	@Column(nullable = false)
	private Integer voorgaandeUitnodigingen = 0;

	@Column(nullable = false)
	private Integer voorgaandeAfspraken = 0;

	@Column(nullable = false)
	private Integer voorgaandeOnderzoeken = 0;

	@Column(nullable = false)
	private Integer voorgaandeMammogrammen = 0;

	@Column(nullable = true)
	private Boolean afgemeldVorigeScreeningRonde;

	@Column(nullable = true)
	@Enumerated(EnumType.STRING)
	private MammaAfspraakStatus afspraakStatusVorigeAfspraak;

	@Column(nullable = true)
	@Enumerated(EnumType.STRING)
	private MammaBeoordelingStatus beoordelingStatusVorigeBeoordeling;

	@Column(nullable = true, precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S5)
	private BigDecimal historieScoreScreeningRonde10Jaar;

	@Column(nullable = true, precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S5)
	private BigDecimal historieScoreScreeningRonde5Jaar;

	@Column(nullable = true, precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S5)
	private BigDecimal historieScoreScreeningRonde3Jaar;

	@Column(nullable = true)
	private Boolean deelnameVorigeScreeningRonde;

	@Column(nullable = true, precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S5)
	private BigDecimal historieScoreAfspraak10Jaar;

	@Column(nullable = true, precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S5)
	private BigDecimal historieScoreAfspraak5Jaar;

	@Column(nullable = true, precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S5)
	private BigDecimal historieScoreAfspraak3Jaar;

	@Column(nullable = true)
	private Boolean opkomstVorigeAfspraak;

	@Column(nullable = false)
	private Long leeftijd;

	@Column(nullable = false)
	private Long leeftijdPer5;

	public void setWijzigingsDatum(Date wijzigingsDatum)
	{
		this.wijzigingsDatum = wijzigingsDatum;
	}

	public void setDoelgroep(MammaDoelgroep doelgroep)
	{
		this.doelgroep = doelgroep;
	}

	public void setTehuis(Boolean tehuis)
	{
		this.tehuis = tehuis;
	}

	public void setVoorgaandeScreeningRondes(Integer voorgaandeScreeningRondes)
	{
		this.voorgaandeScreeningRondes = voorgaandeScreeningRondes;
	}

	public void setVoorgaandeMammogrammen(Integer voorgaandeMammogrammen)
	{
		this.voorgaandeMammogrammen = voorgaandeMammogrammen;
	}

	public void setHistorieScoreScreeningRonde10Jaar(BigDecimal historieScoreScreeningRonde10Jaar)
	{
		this.historieScoreScreeningRonde10Jaar = historieScoreScreeningRonde10Jaar;
	}

	public void setHistorieScoreScreeningRonde5Jaar(BigDecimal historieScoreScreeningRonde5Jaar)
	{
		this.historieScoreScreeningRonde5Jaar = historieScoreScreeningRonde5Jaar;
	}

	public void setOpkomstVorigeAfspraak(Boolean opkomstVorigeAfspraak)
	{
		this.opkomstVorigeAfspraak = opkomstVorigeAfspraak;
	}

	public void setLeeftijd(Long leeftijd)
	{
		this.leeftijd = leeftijd;
	}

	public void setLeeftijdPer5(Long leeftijdPer5)
	{
		this.leeftijdPer5 = leeftijdPer5;
	}

	public void setDeelnameVorigeScreeningRonde(Boolean deelnameVorigeScreeningRonde)
	{
		this.deelnameVorigeScreeningRonde = deelnameVorigeScreeningRonde;
	}

	public Date getWijzigingsDatum()
	{
		return wijzigingsDatum;
	}

	public MammaDoelgroep getDoelgroep()
	{
		return doelgroep;
	}

	public Boolean getTehuis()
	{
		return tehuis;
	}

	public Integer getVoorgaandeScreeningRondes()
	{
		return voorgaandeScreeningRondes;
	}

	public Integer getVoorgaandeMammogrammen()
	{
		return voorgaandeMammogrammen;
	}

	public BigDecimal getHistorieScoreScreeningRonde10Jaar()
	{
		return historieScoreScreeningRonde10Jaar;
	}

	public BigDecimal getHistorieScoreScreeningRonde5Jaar()
	{
		return historieScoreScreeningRonde5Jaar;
	}

	public Boolean getDeelnameVorigeScreeningRonde()
	{
		return deelnameVorigeScreeningRonde;
	}

	public Boolean getOpkomstVorigeAfspraak()
	{
		return opkomstVorigeAfspraak;
	}

	public Long getLeeftijd()
	{
		return leeftijd;
	}

	public Long getLeeftijdPer5()
	{
		return leeftijdPer5;
	}

	public Integer getVoorgaandeAfspraken()
	{
		return voorgaandeAfspraken;
	}

	public void setVoorgaandeAfspraken(Integer voorgaandeAfspraken)
	{
		this.voorgaandeAfspraken = voorgaandeAfspraken;
	}

	public Integer getVoorgaandeOnderzoeken()
	{
		return voorgaandeOnderzoeken;
	}

	public void setVoorgaandeOnderzoeken(Integer voorgaandeOnderzoeken)
	{
		this.voorgaandeOnderzoeken = voorgaandeOnderzoeken;
	}

	public Boolean getAfgemeldVorigeScreeningRonde()
	{
		return afgemeldVorigeScreeningRonde;
	}

	public void setAfgemeldVorigeScreeningRonde(Boolean afgemeldVorigeScreeningRonde)
	{
		this.afgemeldVorigeScreeningRonde = afgemeldVorigeScreeningRonde;
	}

	public Integer getVoorgaandeUitnodigingen()
	{
		return voorgaandeUitnodigingen;
	}

	public void setVoorgaandeUitnodigingen(Integer voorgaandeUitnodigingen)
	{
		this.voorgaandeUitnodigingen = voorgaandeUitnodigingen;
	}

	public BigDecimal getHistorieScoreScreeningRonde3Jaar()
	{
		return historieScoreScreeningRonde3Jaar;
	}

	public void setHistorieScoreScreeningRonde3Jaar(BigDecimal historieScoreScreeningRonde3Jaar)
	{
		this.historieScoreScreeningRonde3Jaar = historieScoreScreeningRonde3Jaar;
	}

	public BigDecimal getHistorieScoreAfspraak10Jaar()
	{
		return historieScoreAfspraak10Jaar;
	}

	public void setHistorieScoreAfspraak10Jaar(BigDecimal historieScoreAfspraak10Jaar)
	{
		this.historieScoreAfspraak10Jaar = historieScoreAfspraak10Jaar;
	}

	public BigDecimal getHistorieScoreAfspraak5Jaar()
	{
		return historieScoreAfspraak5Jaar;
	}

	public void setHistorieScoreAfspraak5Jaar(BigDecimal historieScoreAfspraak5Jaar)
	{
		this.historieScoreAfspraak5Jaar = historieScoreAfspraak5Jaar;
	}

	public BigDecimal getHistorieScoreAfspraak3Jaar()
	{
		return historieScoreAfspraak3Jaar;
	}

	public void setHistorieScoreAfspraak3Jaar(BigDecimal historieScoreAfspraak3Jaar)
	{
		this.historieScoreAfspraak3Jaar = historieScoreAfspraak3Jaar;
	}

	public MammaAfspraakStatus getAfspraakStatusVorigeAfspraak()
	{
		return afspraakStatusVorigeAfspraak;
	}

	public void setAfspraakStatusVorigeAfspraak(MammaAfspraakStatus afspraakStatusVorigeAfspraak)
	{
		this.afspraakStatusVorigeAfspraak = afspraakStatusVorigeAfspraak;
	}

	public MammaBeoordelingStatus getBeoordelingStatusVorigeBeoordeling()
	{
		return beoordelingStatusVorigeBeoordeling;
	}

	public void setBeoordelingStatusVorigeBeoordeling(MammaBeoordelingStatus beoordelingStatusVorigeBeoordeling)
	{
		this.beoordelingStatusVorigeBeoordeling = beoordelingStatusVorigeBeoordeling;
	}

	public Long getPeilEpochDay()
	{
		return peilEpochDay;
	}

	public void setPeilEpochDay(Long peilEpochDay)
	{
		this.peilEpochDay = peilEpochDay;
	}
}
