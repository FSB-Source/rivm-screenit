package nl.rivm.screenit.service.mamma.impl;

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

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;

import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;

public class MammaCapaciteit implements Serializable
{
	Map<MammaCapaciteitBlokType, BlokTypeCapaciteit> capaciteitMap = new HashMap<>();

	public MammaCapaciteit()
	{
		for (MammaCapaciteitBlokType capaciteitBlokType : MammaCapaciteitBlokType.values())
		{
			capaciteitMap.put(capaciteitBlokType, new BlokTypeCapaciteit());
		}
	}

	public class BlokTypeCapaciteit implements Serializable
	{
		public BigDecimal beschikbareCapaciteit = BigDecimal.ZERO;

		public BigDecimal benutteCapaciteit = BigDecimal.ZERO;

		public BigDecimal vrijeCapaciteit = BigDecimal.ZERO;

		public BigDecimal negatieveVrijeCapaciteit = BigDecimal.ZERO;

		public BigDecimal getVrijeCapaciteit(boolean corrigeerNegatieveVrijeCapaciteit)
		{
			return corrigeerNegatieveVrijeCapaciteit ? vrijeCapaciteit.add(negatieveVrijeCapaciteit) : vrijeCapaciteit;
		}
	}

	public BlokTypeCapaciteit getCapaciteit(MammaCapaciteitBlokType... blokTypes)
	{
		BlokTypeCapaciteit blokTypeCapaciteit = new BlokTypeCapaciteit();
		for (MammaCapaciteitBlokType blokType : blokTypes)
		{
			blokTypeCapaciteit.beschikbareCapaciteit = blokTypeCapaciteit.beschikbareCapaciteit.add(capaciteitMap.get(blokType).beschikbareCapaciteit);
			blokTypeCapaciteit.benutteCapaciteit = blokTypeCapaciteit.benutteCapaciteit.add(capaciteitMap.get(blokType).benutteCapaciteit);
			blokTypeCapaciteit.vrijeCapaciteit = blokTypeCapaciteit.vrijeCapaciteit.add(capaciteitMap.get(blokType).vrijeCapaciteit);
			blokTypeCapaciteit.negatieveVrijeCapaciteit = blokTypeCapaciteit.negatieveVrijeCapaciteit.add(capaciteitMap.get(blokType).negatieveVrijeCapaciteit);
		}
		return blokTypeCapaciteit;
	}
}
