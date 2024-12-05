package nl.rivm.screenit.specification.colon;

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

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.AbstractHoudbaarheid_;
import nl.rivm.screenit.model.colon.IFOBTVervaldatum;
import nl.rivm.screenit.specification.ExtendedSpecification;

import org.springframework.data.jpa.domain.Specification;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.RangeSpecification.overlapt;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class ColonFITHoudbaarheidSpecification
{
	public static Specification<IFOBTVervaldatum> heeftBarcodeInRange(String barcode)
	{
		return (r, q, cb) -> cb.and(
			cb.lessThanOrEqualTo(r.get(AbstractHoudbaarheid_.barcodeStart), barcode),
			cb.greaterThanOrEqualTo(r.get(AbstractHoudbaarheid_.barcodeEnd), barcode),
			cb.equal(r.get(AbstractHoudbaarheid_.lengthBarcode), barcode.length()));
	}

	public static ExtendedSpecification<IFOBTVervaldatum> overlaptBarcode(Range<String> barcodeRange)
	{
		return overlapt(barcodeRange, r -> r.get(AbstractHoudbaarheid_.barcodeStart), r -> r.get(AbstractHoudbaarheid_.barcodeEnd));
	}

	public static ExtendedSpecification<IFOBTVervaldatum> heeftBarcodeLengte(int lengte)
	{
		return (r, q, cb) -> cb.equal(r.get(AbstractHoudbaarheid_.lengthBarcode), lengte);
	}
}
