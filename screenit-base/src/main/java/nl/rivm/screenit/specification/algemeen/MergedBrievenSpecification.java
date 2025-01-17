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
import java.util.Collection;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.MergedBrieven_;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNullExtended;
import static nl.rivm.screenit.util.DateUtil.toUtilDate;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MergedBrievenSpecification
{
	public static <M extends MergedBrieven<?>> Specification<M> heeftBriefTypeIn(Collection<BriefType> briefTypes)
	{
		return (r, q, cb) -> r.get(MergedBrieven_.briefType).in(briefTypes);
	}

	public static <M extends MergedBrieven<?>> ExtendedSpecification<M> heeftPrintDatum()
	{
		return (r, q, cb) -> cb.isNotNull(r.get(MergedBrieven_.printDatum));
	}

	public static <M extends MergedBrieven<?>> ExtendedSpecification<M> heeftPrintDatumVoor(LocalDate peilDatum)
	{
		return (r, q, cb) -> cb.lessThan(r.get(MergedBrieven_.printDatum), DateUtil.toUtilDate(peilDatum));
	}

	public static <M extends MergedBrieven<?>> Specification<M> heeftCreatieDatumVoorOfOp(LocalDateTime peilMoment)
	{
		return (r, q, cb) -> cb.lessThanOrEqualTo(r.get(MergedBrieven_.creatieDatum), toUtilDate(peilMoment));
	}

	public static <M extends MergedBrieven<?>> ExtendedSpecification<M> isVerwijderd(boolean waarde)
	{
		return (r, q, cb) -> cb.equal(r.get(MergedBrieven_.verwijderd), waarde);
	}

	public static <M extends MergedBrieven<?>> ExtendedSpecification<M> isVrijgegeven(boolean waarde)
	{
		return (r, q, cb) -> cb.equal(r.get(MergedBrieven_.vrijgegeven), waarde);
	}

	public static <M extends MergedBrieven<?>> ExtendedSpecification<M> heeftGeenBriefType()
	{
		return (r, q, cb) -> cb.isNull(r.get(MergedBrieven_.briefType));
	}

	public static <M extends MergedBrieven<?>> ExtendedSpecification<M> filterScreeningOrganisatie(ScreeningOrganisatie screeningOrganisatie)
	{
		return skipWhenNullExtended(screeningOrganisatie, (r, q, cb) -> cb.equal(r.get(MergedBrieven_.screeningOrganisatie), screeningOrganisatie));
	}

	public static <M extends MergedBrieven<?>> ExtendedSpecification<M> filterGeprint(Boolean waarde)
	{
		return skipWhenNullExtended(waarde, (r, q, cb) -> cb.equal(r.get(MergedBrieven_.geprint), waarde));
	}

	public static <M extends MergedBrieven<?>> ExtendedSpecification<M> filterControle(Boolean waarde)
	{
		return skipWhenNullExtended(waarde, (r, q, cb) -> cb.equal(r.get(MergedBrieven_.controle), waarde));
	}

}
