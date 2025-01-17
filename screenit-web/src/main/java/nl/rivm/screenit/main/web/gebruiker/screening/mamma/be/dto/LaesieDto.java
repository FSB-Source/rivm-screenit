package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.dto;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.Serializable;
import java.math.BigDecimal;

import nl.rivm.screenit.model.mamma.enums.MammaAsymmetrieSpecificatie;
import nl.rivm.screenit.model.mamma.enums.MammaCalcificatiesDistributie;
import nl.rivm.screenit.model.mamma.enums.MammaCalcificatiesVorm;
import nl.rivm.screenit.model.mamma.enums.MammaLaesieType;
import nl.rivm.screenit.model.mamma.enums.MammaMassaBegrenzing;
import nl.rivm.screenit.model.mamma.enums.MammaMassaDensiteit;
import nl.rivm.screenit.model.mamma.enums.MammaMassaVorm;
import nl.rivm.screenit.model.mamma.enums.MammaZijde;

public class LaesieDto implements Serializable
{
	private MammaLaesieType laesietype;

	private MammaZijde welkeBorst;

	private MammaLaesieIcoonDto verticaleDoorsnede;

	private MammaLaesieIcoonDto horizontaleDoorsnede;

	private MammaAsymmetrieSpecificatie asymmetrieSpecificatie;

	private MammaMassaVorm massaVorm;

	private MammaMassaBegrenzing massaBegrenzing;

	private MammaMassaDensiteit massaDensiteit;

	private MammaCalcificatiesVorm calcificatiesVorm;

	private MammaCalcificatiesDistributie calcificatiesDistributie;

	private int nummer;

	private BigDecimal laesieGrootteInCm;

	public MammaLaesieType getLaesietype()
	{
		return laesietype;
	}

	public void setLaesietype(MammaLaesieType laesietype)
	{
		this.laesietype = laesietype;
	}

	public MammaZijde getWelkeBorst()
	{
		return welkeBorst;
	}

	public void setWelkeBorst(MammaZijde welkeBorst)
	{
		this.welkeBorst = welkeBorst;
	}

	public MammaLaesieIcoonDto getVerticaleDoorsnede()
	{
		return verticaleDoorsnede;
	}

	public void setVerticaleDoorsnede(MammaLaesieIcoonDto verticaleDoorsnede)
	{
		this.verticaleDoorsnede = verticaleDoorsnede;
	}

	public MammaLaesieIcoonDto getHorizontaleDoorsnede()
	{
		return horizontaleDoorsnede;
	}

	public void setHorizontaleDoorsnede(MammaLaesieIcoonDto horizontaleDoorsnede)
	{
		this.horizontaleDoorsnede = horizontaleDoorsnede;
	}

	public int getNummer()
	{
		return nummer;
	}

	public void setNummer(int nummer)
	{
		this.nummer = nummer;
	}

	public MammaAsymmetrieSpecificatie getAsymmetrieSpecificatie()
	{
		return asymmetrieSpecificatie;
	}

	public void setAsymmetrieSpecificatie(MammaAsymmetrieSpecificatie asymmetrieSpecificatie)
	{
		this.asymmetrieSpecificatie = asymmetrieSpecificatie;
	}

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

	public BigDecimal getLaesieGrootteInCm()
	{
		return laesieGrootteInCm;
	}

	public void setLaesieGrootteInCm(BigDecimal laesieGrootteInCm)
	{
		this.laesieGrootteInCm = laesieGrootteInCm;
	}
}
