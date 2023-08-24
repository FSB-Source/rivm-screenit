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

import java.io.Serializable;
import java.math.BigDecimal;

import javax.persistence.Column;
import javax.persistence.Embeddable;

import nl.rivm.screenit.model.helper.HibernateMagicNumber;

@Embeddable
public class MammaLaesieIcoon implements Serializable
{
	@Column(precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S3) 
	private BigDecimal positieX;

	@Column(precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S3)
	private BigDecimal positieY;

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
}
