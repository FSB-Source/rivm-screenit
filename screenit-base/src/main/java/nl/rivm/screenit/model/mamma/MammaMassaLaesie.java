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

import javax.persistence.Column;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;

import nl.rivm.screenit.model.mamma.enums.MammaLaesieType;
import nl.rivm.screenit.model.mamma.enums.MammaMassaBegrenzing;
import nl.rivm.screenit.model.mamma.enums.MammaMassaDensiteit;
import nl.rivm.screenit.model.mamma.enums.MammaMassaVorm;

import org.hibernate.envers.Audited;

@Entity
@DiscriminatorValue("massa")
@Audited
public class MammaMassaLaesie extends MammaLaesie
{
	@Override
	public MammaLaesieType getMammaLaesieType()
	{
		return MammaLaesieType.MASSA;
	}

	@Column()
	@Enumerated(EnumType.STRING)
	private MammaMassaVorm massaVorm;

	@Column()
	@Enumerated(EnumType.STRING)
	private MammaMassaBegrenzing massaBegrenzing;

	@Column()
	@Enumerated(EnumType.STRING)
	private MammaMassaDensiteit massaDensiteit;

	public MammaMassaVorm getMassaVorm()
	{
		return massaVorm;
	}

	public void setMassaVorm(MammaMassaVorm massaVorm)
	{
		this.massaVorm = massaVorm;
	}

	public MammaMassaBegrenzing getMassaBegrenzing()
	{
		return massaBegrenzing;
	}

	public void setMassaBegrenzing(MammaMassaBegrenzing massaBegrenzing)
	{
		this.massaBegrenzing = massaBegrenzing;
	}

	public MammaMassaDensiteit getMassaDensiteit()
	{
		return massaDensiteit;
	}

	public void setMassaDensiteit(MammaMassaDensiteit massaDensiteit)
	{
		this.massaDensiteit = massaDensiteit;
	}
}
