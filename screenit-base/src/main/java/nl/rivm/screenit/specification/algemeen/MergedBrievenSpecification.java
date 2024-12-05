package nl.rivm.screenit.specification.algemeen;

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

import java.time.LocalDate;
import java.util.Collection;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.MergedBrieven_;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.jpa.domain.Specification;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
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

	public static <M extends MergedBrieven<?>> ExtendedSpecification<M> heeftPrintDatumVoor(LocalDate peilmoment)
	{
		return (r, q, cb) -> cb.lessThan(r.get(MergedBrieven_.printDatum), DateUtil.toUtilDate(peilmoment));
	}
}
