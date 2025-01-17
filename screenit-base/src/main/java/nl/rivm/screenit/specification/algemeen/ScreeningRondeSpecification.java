package nl.rivm.screenit.specification.algemeen;

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

import java.time.LocalDate;
import java.time.LocalDateTime;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.ScreeningRonde_;
import nl.rivm.screenit.specification.DateSpecification;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.util.DateUtil;

import com.google.common.collect.Range;

import static nl.rivm.screenit.model.ScreeningRondeStatus.AFGEROND;
import static nl.rivm.screenit.model.ScreeningRondeStatus.LOPEND;
import static nl.rivm.screenit.specification.DateSpecification.bevatLocalDateToDate;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ScreeningRondeSpecification
{
	public static <S extends ScreeningRonde<?, ?, ?, ?>> ExtendedSpecification<S> isLopend()
	{
		return heeftStatus(LOPEND);
	}

	public static <S extends ScreeningRonde<?, ?, ?, ?>> ExtendedSpecification<S> isAfgerond()
	{
		return heeftStatus(AFGEROND);
	}

	private static <S extends ScreeningRonde<?, ?, ?, ?>> ExtendedSpecification<S> heeftStatus(ScreeningRondeStatus status)
	{
		return (r, q, cb) -> cb.equal(r.get(ScreeningRonde_.status), status);
	}

	public static <S extends ScreeningRonde<?, ?, ?, ?>> ExtendedSpecification<S> heeftStatusDatumOpOfVoor(LocalDateTime peilMoment)
	{
		return (r, q, cb) -> cb.lessThanOrEqualTo(r.get(ScreeningRonde_.statusDatum), DateUtil.toUtilDate(peilMoment));
	}

	public static <S extends ScreeningRonde<?, ?, ?, ?>> ExtendedSpecification<S> isAangemaaktVoor(LocalDate datum)
	{
		return (r, q, cb) -> cb.lessThan(r.get(ScreeningRonde_.creatieDatum), DateUtil.toUtilDate(datum));
	}

	public static <S extends ScreeningRonde<?, ?, ?, ?>> ExtendedSpecification<S> isAangemaaktVoor(LocalDateTime datum)
	{
		return (r, q, cb) -> cb.lessThan(r.get(ScreeningRonde_.creatieDatum), DateUtil.toUtilDate(datum));
	}

	public static <S extends ScreeningRonde<?, ?, ?, ?>> ExtendedSpecification<S> isAangemaaktIn(Range<LocalDate> bereik)
	{
		return bevatLocalDateToDate(bereik, r -> r.get(ScreeningRonde_.creatieDatum));
	}
}
