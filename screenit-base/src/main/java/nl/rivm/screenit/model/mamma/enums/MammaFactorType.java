package nl.rivm.screenit.model.mamma.enums;

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

import nl.rivm.screenit.model.ScreeningOrganisatie;

public enum MammaFactorType
{
	GEEN,
	EERSTE_ONDERZOEK,
	DUBBELE_TIJD,
	MINDER_VALIDE;

	public BigDecimal getFactor(ScreeningOrganisatie screeningOrganisatie)
	{
		switch (this)
		{
		case GEEN:
			return BigDecimal.ONE;
		case EERSTE_ONDERZOEK:
			return screeningOrganisatie.getFactorEersteOnderzoekBk();
		case DUBBELE_TIJD:
			return screeningOrganisatie.getFactorDubbeleTijdBk();
		case MINDER_VALIDE:
			return screeningOrganisatie.getFactorMinderValideBk();
		default:
			throw new IllegalStateException("Unexpected value: " + this);
		}
	}

	public static MammaFactorType getFactorType(boolean isTehuisClient, MammaDoelgroep doelgroep, boolean heeftOoitUitslagGehad)
	{
		if (isTehuisClient)
		{
			return doelgroep.equals(MammaDoelgroep.MINDER_VALIDE) ? MINDER_VALIDE : DUBBELE_TIJD;
		}

		switch (doelgroep)
		{
		case REGULIER:
			return heeftOoitUitslagGehad ? GEEN : EERSTE_ONDERZOEK;
		case DUBBELE_TIJD:
			return DUBBELE_TIJD;
		case MINDER_VALIDE:
			return MINDER_VALIDE;
		default:
			throw new IllegalStateException("Unexpected value: " + doelgroep);
		}
	}
}
