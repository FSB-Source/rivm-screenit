package nl.rivm.screenit.service.impl;

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

import nl.rivm.screenit.model.DigitaalClientBericht;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.service.DigitaalClientBerichtService;
import nl.rivm.screenit.service.mamma.MammaDigitaalClientBerichtService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Primary;
import org.springframework.stereotype.Service;

@SuppressWarnings("unchecked")
@Primary
@Service
public class DigitaalClientBerichtServiceImpl implements DigitaalClientBerichtService
{
	@Autowired
	private MammaDigitaalClientBerichtService mammaDigitaalClientBerichtService;

	@Override
	public void saveOrUpdate(DigitaalClientBericht digitaalClientBericht)
	{
		bvoDigitaalClientBerichtService(digitaalClientBericht).saveOrUpdate(digitaalClientBericht);
	}

	@Override
	public boolean digitaalClientBerichtMagOpnieuwVerzondenWorden(DigitaalClientBericht digitaalClientBericht)
	{
		return bvoDigitaalClientBerichtService(digitaalClientBericht).digitaalClientBerichtMagOpnieuwVerzondenWorden(digitaalClientBericht);
	}

	private DigitaalClientBerichtService bvoDigitaalClientBerichtService(DigitaalClientBericht digitaalClientBericht)
	{
		if (Bevolkingsonderzoek.MAMMA == digitaalClientBericht.getScreeningRonde().getBevolkingsonderzoek())
		{
			return mammaDigitaalClientBerichtService;
		}
		else
		{
			throw new IllegalStateException();
		}
	}
}
