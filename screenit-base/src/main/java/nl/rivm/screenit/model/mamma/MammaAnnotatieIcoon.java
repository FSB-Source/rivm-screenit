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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.mamma.enums.MammaAnnotatieIcoonType;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "mamma", name = "annotatie_icoon")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
public class MammaAnnotatieIcoon extends AbstractHibernateObject
{
	private static final long serialVersionUID = 1L;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private MammaAnnotatieAfbeelding afbeelding;

	@Column(nullable = false, precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S3)
	private BigDecimal positieX;

	@Column(nullable = false, precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S3)
	private BigDecimal positieY;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaAnnotatieIcoonType type;

	@Column
	private String tekst;

	public MammaAnnotatieAfbeelding getAfbeelding()
	{
		return afbeelding;
	}

	public void setAfbeelding(MammaAnnotatieAfbeelding afbeelding)
	{
		this.afbeelding = afbeelding;
	}

	public BigDecimal getPositieX()
	{
		return positieX;
	}

	public void setPositieX(BigDecimal positieX)
	{
		this.positieX = positieX;
	}

	public BigDecimal getPositieY()
	{
		return positieY;
	}

	public void setPositieY(BigDecimal positieY)
	{
		this.positieY = positieY;
	}

	public MammaAnnotatieIcoonType getType()
	{
		return type;
	}

	public void setType(MammaAnnotatieIcoonType type)
	{
		this.type = type;
	}

	public String getTekst()
	{
		return tekst;
	}

	public void setTekst(String tekst)
	{
		this.tekst = tekst;
	}
}
