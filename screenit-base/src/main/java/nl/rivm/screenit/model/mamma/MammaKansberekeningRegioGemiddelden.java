package nl.rivm.screenit.model.mamma;

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

import java.math.BigDecimal;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.mamma.enums.MammaRegioType;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Table(
	schema = "mamma",
	name = "kansberekening_regio_gemiddelden",
	uniqueConstraints = { @UniqueConstraint(columnNames = "regio") })
@Audited
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
public class MammaKansberekeningRegioGemiddelden extends AbstractHibernateObject
{
	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date wijzigingsDatum;

	@Column(nullable = true)
	private String regio;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaRegioType regioType;

	@Column(nullable = true, precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S5)
	private BigDecimal deelnameAfgelopen3Jaar;

	@Column(nullable = true, precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S5)
	private BigDecimal deelnameAfgelopen5Jaar;

	@Column(nullable = true, precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S5)
	private BigDecimal deelnameAfgelopen10Jaar;

	@Column(nullable = true, precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S5)
	private BigDecimal deelnameEersteRondeAfgelopen3Jaar;

	@Column(nullable = true, precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S5)
	private BigDecimal deelnameEersteRondeAfgelopen5Jaar;

	@Column(nullable = true, precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S5)
	private BigDecimal deelnameEersteRondeAfgelopen10Jaar;

	@Column(nullable = true, precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S5)
	private BigDecimal opkomstAfgelopen3Jaar;

	@Column(nullable = true, precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S5)
	private BigDecimal opkomstAfgelopen5Jaar;

	@Column(nullable = true, precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S5)
	private BigDecimal opkomstAfgelopen10Jaar;

	@Column(nullable = true, precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S5)
	private BigDecimal opkomstEersteRondeAfgelopen3Jaar;

	@Column(nullable = true, precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S5)
	private BigDecimal opkomstEersteRondeAfgelopen5Jaar;

	@Column(nullable = true, precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S5)
	private BigDecimal opkomstEersteRondeAfgelopen10Jaar;

	public Date getWijzigingsDatum()
	{
		return wijzigingsDatum;
	}

	public void setWijzigingsDatum(Date wijzigingsDatum)
	{
		this.wijzigingsDatum = wijzigingsDatum;
	}

	public String getRegio()
	{
		return regio;
	}

	public void setRegio(String regio)
	{
		this.regio = regio;
	}

	public BigDecimal getDeelnameAfgelopen3Jaar()
	{
		return deelnameAfgelopen3Jaar;
	}

	public void setDeelnameAfgelopen3Jaar(BigDecimal deelnameAfgelopen3Jaar)
	{
		this.deelnameAfgelopen3Jaar = deelnameAfgelopen3Jaar;
	}

	public BigDecimal getDeelnameAfgelopen5Jaar()
	{
		return deelnameAfgelopen5Jaar;
	}

	public void setDeelnameAfgelopen5Jaar(BigDecimal deelnameAfgelopen5Jaar)
	{
		this.deelnameAfgelopen5Jaar = deelnameAfgelopen5Jaar;
	}

	public BigDecimal getDeelnameAfgelopen10Jaar()
	{
		return deelnameAfgelopen10Jaar;
	}

	public void setDeelnameAfgelopen10Jaar(BigDecimal deelnameAfgelopen10Jaar)
	{
		this.deelnameAfgelopen10Jaar = deelnameAfgelopen10Jaar;
	}

	public BigDecimal getDeelnameEersteRondeAfgelopen3Jaar()
	{
		return deelnameEersteRondeAfgelopen3Jaar;
	}

	public void setDeelnameEersteRondeAfgelopen3Jaar(BigDecimal deelnameEersteRondeAfgelopen3Jaar)
	{
		this.deelnameEersteRondeAfgelopen3Jaar = deelnameEersteRondeAfgelopen3Jaar;
	}

	public BigDecimal getDeelnameEersteRondeAfgelopen5Jaar()
	{
		return deelnameEersteRondeAfgelopen5Jaar;
	}

	public void setDeelnameEersteRondeAfgelopen5Jaar(BigDecimal deelnameEersteRondeAfgelopen5Jaar)
	{
		this.deelnameEersteRondeAfgelopen5Jaar = deelnameEersteRondeAfgelopen5Jaar;
	}

	public BigDecimal getDeelnameEersteRondeAfgelopen10Jaar()
	{
		return deelnameEersteRondeAfgelopen10Jaar;
	}

	public void setDeelnameEersteRondeAfgelopen10Jaar(BigDecimal deelnameEersteRondeAfgelopen10Jaar)
	{
		this.deelnameEersteRondeAfgelopen10Jaar = deelnameEersteRondeAfgelopen10Jaar;
	}

	public BigDecimal getOpkomstAfgelopen3Jaar()
	{
		return opkomstAfgelopen3Jaar;
	}

	public void setOpkomstAfgelopen3Jaar(BigDecimal opkomstAfgelopen3Jaar)
	{
		this.opkomstAfgelopen3Jaar = opkomstAfgelopen3Jaar;
	}

	public BigDecimal getOpkomstAfgelopen5Jaar()
	{
		return opkomstAfgelopen5Jaar;
	}

	public void setOpkomstAfgelopen5Jaar(BigDecimal opkomstAfgelopen5Jaar)
	{
		this.opkomstAfgelopen5Jaar = opkomstAfgelopen5Jaar;
	}

	public BigDecimal getOpkomstAfgelopen10Jaar()
	{
		return opkomstAfgelopen10Jaar;
	}

	public void setOpkomstAfgelopen10Jaar(BigDecimal opkomstAfgelopen10Jaar)
	{
		this.opkomstAfgelopen10Jaar = opkomstAfgelopen10Jaar;
	}

	public BigDecimal getOpkomstEersteRondeAfgelopen3Jaar()
	{
		return opkomstEersteRondeAfgelopen3Jaar;
	}

	public void setOpkomstEersteRondeAfgelopen3Jaar(BigDecimal opkomstEersteRondeAfgelopen3Jaar)
	{
		this.opkomstEersteRondeAfgelopen3Jaar = opkomstEersteRondeAfgelopen3Jaar;
	}

	public BigDecimal getOpkomstEersteRondeAfgelopen5Jaar()
	{
		return opkomstEersteRondeAfgelopen5Jaar;
	}

	public void setOpkomstEersteRondeAfgelopen5Jaar(BigDecimal opkomstEersteRondeAfgelopen5Jaar)
	{
		this.opkomstEersteRondeAfgelopen5Jaar = opkomstEersteRondeAfgelopen5Jaar;
	}

	public BigDecimal getOpkomstEersteRondeAfgelopen10Jaar()
	{
		return opkomstEersteRondeAfgelopen10Jaar;
	}

	public void setOpkomstEersteRondeAfgelopen10Jaar(BigDecimal opkomstEersteRondeAfgelopen10Jaar)
	{
		this.opkomstEersteRondeAfgelopen10Jaar = opkomstEersteRondeAfgelopen10Jaar;
	}

	public MammaRegioType getRegioType()
	{
		return regioType;
	}

	public void setRegioType(MammaRegioType regioType)
	{
		this.regioType = regioType;
	}
}
