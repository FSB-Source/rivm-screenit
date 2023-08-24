package nl.rivm.screenit.model.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Table(
	schema = "mamma",
	name = "kansberekening_standplaats_ronde_gemiddelden",
	uniqueConstraints = { @UniqueConstraint(columnNames = "standplaats_ronde") })
@Audited
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
public class MammaKansberekeningStandplaatsRondeGemiddelden extends AbstractHibernateObject
{
	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date wijzigingsDatum;

	@OneToOne(fetch = FetchType.LAZY, optional = false)
	private MammaStandplaatsRonde standplaatsRonde;

	@Column(nullable = true, precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S5)
	private BigDecimal deelname;

	@Column(nullable = true, precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S5)
	private BigDecimal deelnameEersteRonde;

	@Column(nullable = true, precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S5)
	private BigDecimal opkomst;

	@Column(nullable = true, precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S5)
	private BigDecimal opkomstEersteRonde;

	public Date getWijzigingsDatum()
	{
		return wijzigingsDatum;
	}

	public void setWijzigingsDatum(Date wijzigingsDatum)
	{
		this.wijzigingsDatum = wijzigingsDatum;
	}

	public MammaStandplaatsRonde getStandplaatsRonde()
	{
		return standplaatsRonde;
	}

	public void setStandplaatsRonde(MammaStandplaatsRonde standplaatsRonde)
	{
		this.standplaatsRonde = standplaatsRonde;
	}

	public BigDecimal getDeelname()
	{
		return deelname;
	}

	public void setDeelname(BigDecimal deelname)
	{
		this.deelname = deelname;
	}

	public BigDecimal getDeelnameEersteRonde()
	{
		return deelnameEersteRonde;
	}

	public void setDeelnameEersteRonde(BigDecimal deelnameEersteRonde)
	{
		this.deelnameEersteRonde = deelnameEersteRonde;
	}

	public BigDecimal getOpkomst()
	{
		return opkomst;
	}

	public void setOpkomst(BigDecimal opkomst)
	{
		this.opkomst = opkomst;
	}

	public BigDecimal getOpkomstEersteRonde()
	{
		return opkomstEersteRonde;
	}

	public void setOpkomstEersteRonde(BigDecimal opkomstEersteRonde)
	{
		this.opkomstEersteRonde = opkomstEersteRonde;
	}
}
