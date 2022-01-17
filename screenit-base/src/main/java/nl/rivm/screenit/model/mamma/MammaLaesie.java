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

import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.SingleTableHibernateObject;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.mamma.enums.MammaLaesieType;
import nl.rivm.screenit.model.mamma.enums.MammaZijde;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Check;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "mamma", name = "laesie")
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
@Audited
@Check(constraints = "laesie.laesie_grootte_in_cm >= 0")
public abstract class MammaLaesie extends SingleTableHibernateObject
{
	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private MammaLezing lezing;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaZijde mammaZijde;

	@Embedded
	@AttributeOverrides({
		@AttributeOverride(name = "positieX", column = @Column(name = "verticaleDoorsnedeIcoonPositieX", precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S3)),
		@AttributeOverride(name = "positieY", column = @Column(name = "verticaleDoorsnedeIcoonPositieY", precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S3))
	})
	private MammaLaesieIcoon verticaleDoorsnedeIcoon;

	@Embedded
	@AttributeOverrides({
		@AttributeOverride(name = "positieX", column = @Column(name = "horizontaleDoorsnedeIcoonPositieX", precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S3)),
		@AttributeOverride(name = "positieY", column = @Column(name = "horizontaleDoorsnedeIcoonPositieY", precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S3))
	})
	private MammaLaesieIcoon horizontaleDoorsnedeIcoon;

	@Column(nullable = false)
	private Integer nummer;

	@Column(precision = 5, scale = 1)
	private BigDecimal laesieGrootteInCm;

	public MammaLezing getLezing()
	{
		return lezing;
	}

	public void setLezing(MammaLezing lezing)
	{
		this.lezing = lezing;
	}

	public MammaLaesieIcoon getVerticaleDoorsnedeIcoon()
	{
		return verticaleDoorsnedeIcoon;
	}

	public void setVerticaleDoorsnedeIcoon(MammaLaesieIcoon verticaleDoorsnedeIcoon)
	{
		this.verticaleDoorsnedeIcoon = verticaleDoorsnedeIcoon;
	}

	public MammaLaesieIcoon getHorizontaleDoorsnedeIcoon()
	{
		return horizontaleDoorsnedeIcoon;
	}

	public void setHorizontaleDoorsnedeIcoon(MammaLaesieIcoon horizontaleDoorsnedeIcoon)
	{
		this.horizontaleDoorsnedeIcoon = horizontaleDoorsnedeIcoon;
	}

	public int getNummer()
	{
		return nummer;
	}

	public void setNummer(int nummer)
	{
		this.nummer = nummer;
	}

	public MammaZijde getMammaZijde()
	{
		return mammaZijde;
	}

	public void setMammaZijde(MammaZijde mammaZijde)
	{
		this.mammaZijde = mammaZijde;
	}

	public abstract MammaLaesieType getMammaLaesieType();

	public void setNummer(Integer nummer)
	{
		this.nummer = nummer;
	}

	public BigDecimal getLaesieGrootteInCm()
	{
		return laesieGrootteInCm;
	}

	public void setLaesieGrootteInCm(BigDecimal newValue)
	{
		this.laesieGrootteInCm = newValue;
	}
}
