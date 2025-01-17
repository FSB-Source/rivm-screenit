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
import java.util.Date;
import java.util.List;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.Brief_;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.util.DateUtil;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class BriefSpecification
{
	public static <B extends Brief> ExtendedSpecification<B> heeftNietBriefType(BriefType briefType)
	{
		return (r, q, cb) -> cb.notEqual(r.get(Brief_.briefType), briefType);
	}

	public static <B extends Brief> ExtendedSpecification<B> heeftBriefType(BriefType briefType)
	{
		return (r, q, cb) -> cb.equal(r.get(Brief_.briefType), briefType);
	}

	public static <B extends Brief> ExtendedSpecification<B> heeftBriefTypeIn(List<BriefType> briefTypes)
	{
		return (r, q, cb) -> r.get(Brief_.briefType).in(briefTypes);
	}

	public static <B extends Brief> ExtendedSpecification<B> isNietVervangen()
	{
		return (r, q, cb) -> cb.isFalse(r.get(Brief_.vervangen));
	}

	public static <B extends Brief> ExtendedSpecification<B> isNietGegenereerd()
	{
		return (r, q, cb) -> cb.isFalse(r.get(Brief_.gegenereerd));
	}

	public static <B extends Brief> ExtendedSpecification<B> isNietTegengehouden()
	{
		return (r, q, cb) -> cb.isFalse(r.get(Brief_.tegenhouden));
	}

	public static <B extends Brief> ExtendedSpecification<B> isGegenereerd(boolean isGegenereerd)
	{
		return (r, q, cb) -> cb.equal(r.get(Brief_.gegenereerd), isGegenereerd);
	}

	public static <B extends Brief> ExtendedSpecification<B> isAangemaaktVoor(LocalDate peilmoment)
	{
		return (r, q, cb) -> cb.lessThan(r.get(Brief_.creatieDatum), DateUtil.toUtilDate(peilmoment));
	}

	public static <B extends Brief> ExtendedSpecification<B> isAangemaaktVoorOfOp(Date peildatum)
	{
		return (r, q, cb) -> cb.lessThanOrEqualTo(r.get(Brief_.creatieDatum), peildatum);
	}

}
