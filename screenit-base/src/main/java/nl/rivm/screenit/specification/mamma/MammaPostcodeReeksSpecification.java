package nl.rivm.screenit.specification.mamma;

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
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.mamma.MammaPostcodeReeks;
import nl.rivm.screenit.model.mamma.MammaPostcodeReeks_;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.specification.ExtendedSpecification;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.math.NumberUtils;
import org.springframework.data.util.Pair;

import com.google.common.collect.Range;

import static com.google.common.collect.BoundType.CLOSED;
import static nl.rivm.screenit.specification.RangeSpecification.bevat;
import static nl.rivm.screenit.specification.RangeSpecification.overlapt;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaPostcodeReeksSpecification
{
	public static ExtendedSpecification<MammaPostcodeReeks> bevatPostcode(String postcode)
	{
		return bevat(r -> r.get(MammaPostcodeReeks_.vanPostcode), r -> r.get(MammaPostcodeReeks_.totPostcode),
			Pair.of(CLOSED, CLOSED), postcode);
	}

	public static ExtendedSpecification<MammaPostcodeReeks> heeftOverlapMetReeks(Range<String> postcodeRange)
	{
		return overlapt(postcodeRange, r -> r.get(MammaPostcodeReeks_.vanPostcode), r -> r.get(MammaPostcodeReeks_.totPostcode));
	}

	public static ExtendedSpecification<MammaPostcodeReeks> heeftStandplaats(MammaStandplaats standplaats)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaPostcodeReeks_.standplaats), standplaats);
	}

	public static ExtendedSpecification<MammaPostcodeReeks> filterOpPostcode4OfPostcode6(String postcode4Of6)
	{
		if (StringUtils.isBlank(postcode4Of6))
		{
			return (r, q, cb) -> null;
		}
		var vanPostcode = postcode4Of6.toUpperCase();
		var totPostcode = vanPostcode;
		if (vanPostcode.length() == 4 && NumberUtils.isNumber(vanPostcode))
		{
			vanPostcode += "AA";
			totPostcode += "ZZ";
		}
		var postcodeRange = Range.closed(vanPostcode, totPostcode);
		return heeftOverlapMetReeks(postcodeRange);
	}
}
