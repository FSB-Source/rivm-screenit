package nl.rivm.screenit.model.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import javax.persistence.Column;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;

import nl.rivm.screenit.model.mamma.enums.MammaCalcificatiesDistributie;
import nl.rivm.screenit.model.mamma.enums.MammaCalcificatiesVorm;
import nl.rivm.screenit.model.mamma.enums.MammaLaesieType;

import org.hibernate.envers.Audited;

@Entity
@DiscriminatorValue("calcificaties")
@Audited
public class MammaCalcificatiesLaesie extends MammaLaesie
{
	@Override
	public MammaLaesieType getMammaLaesieType()
	{
		return MammaLaesieType.CALCIFICATIES;
	}

	@Column()
	@Enumerated(EnumType.STRING)
	private MammaCalcificatiesVorm calcificatiesVorm;

	@Column()
	@Enumerated(EnumType.STRING)
	private MammaCalcificatiesDistributie calcificatiesDistributie;

	public MammaCalcificatiesVorm getCalcificatiesVorm()
	{
		return calcificatiesVorm;
	}

	public void setCalcificatiesVorm(MammaCalcificatiesVorm calcificatiesVorm)
	{
		this.calcificatiesVorm = calcificatiesVorm;
	}

	public MammaCalcificatiesDistributie getCalcificatiesDistributie()
	{
		return calcificatiesDistributie;
	}

	public void setCalcificatiesDistributie(MammaCalcificatiesDistributie calcificatiesDistributie)
	{
		this.calcificatiesDistributie = calcificatiesDistributie;
	}
}
