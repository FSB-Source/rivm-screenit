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

import javax.persistence.Column;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;

import nl.rivm.screenit.model.mamma.enums.MammaAsymmetrieSpecificatie;
import nl.rivm.screenit.model.mamma.enums.MammaLaesieType;

import org.hibernate.envers.Audited;

@Entity
@DiscriminatorValue("asymmetrie")
@Audited
public class MammaAsymmetrieLaesie extends MammaLaesie
{
	@Override
	public MammaLaesieType getMammaLaesieType()
	{
		return MammaLaesieType.ASYMMETRIE;
	}

	@Column()
	@Enumerated(EnumType.STRING)
	private MammaAsymmetrieSpecificatie asymmetrieSpecificatie;

	public MammaAsymmetrieSpecificatie getAsymmetrieSpecificatie()
	{
		return asymmetrieSpecificatie;
	}

	public void setAsymmetrieSpecificatie(MammaAsymmetrieSpecificatie asymmetrieSpecificatie)
	{
		this.asymmetrieSpecificatie = asymmetrieSpecificatie;
	}
}
