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

import java.util.Date;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.Gemeente_;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.RangeSpecification;
import nl.rivm.screenit.util.DateUtil;

import org.apache.commons.lang.StringUtils;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.util.Pair;

import com.google.common.collect.BoundType;

import static nl.rivm.screenit.specification.SpecificationUtil.containsCaseInsensitive;
import static nl.rivm.screenit.specification.SpecificationUtil.exactCaseInsensitive;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNull;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class GemeenteSpecification
{
	public static Specification<Gemeente> heeftNaamContaining(String naam)
	{
		return (r, q, cb) ->
		{
			if (StringUtils.isBlank(naam))
			{
				return cb.like(r.get(Gemeente_.naam), "123456");
			}
			else
			{
				return containsCaseInsensitive(cb, r.get(Gemeente_.naam), naam);
			}
		};
	}

	public static ExtendedSpecification<Gemeente> heeftNaam(String naam)
	{
		return (r, q, cb) -> exactCaseInsensitive(cb, r.get(Gemeente_.naam), naam);
	}

	public static ExtendedSpecification<Gemeente> heeftScreeningOrganisatie()
	{
		return (r, q, cb) -> cb.isNotNull(r.get(Gemeente_.screeningOrganisatie));
	}

	public static Specification<Gemeente> filterScreeningOrganisatie(ScreeningOrganisatie screeningOrganisatie)
	{
		return skipWhenNull(screeningOrganisatie, (r, q, cb) -> cb.equal(r.get(Gemeente_.screeningOrganisatie), screeningOrganisatie));
	}

	public static Specification<Gemeente> heeftNaamEnScreeningOrganisatie(Gemeente gemeente)
	{
		return heeftNaamContaining(gemeente.getNaam())
			.and(filterScreeningOrganisatie(gemeente.getScreeningOrganisatie()));
	}

	public static Specification<Gemeente> heeftGeenBMHKLaboratoriumOfGekoppeldAan(BMHKLaboratorium bmhkLaboratorium)
	{
		return (r, q, cb) ->
			cb.or(cb.isNull(r.get(Gemeente_.bmhkLaboratorium)), cb.equal(r.get(Gemeente_.bmhkLaboratorium), bmhkLaboratorium));
	}

	public static ExtendedSpecification<Gemeente> heeftBmhkLaboratorium()
	{
		return (r, q, cb) -> cb.isNotNull(r.get(Gemeente_.bmhkLaboratorium));
	}

	public static Specification<Gemeente> heeftGeenScreeningOrganisatieOfGekoppeldAan(ScreeningOrganisatie screeningOrganisatie)
	{
		return (r, q, cb) ->
			cb.or(cb.isNull(r.get(Gemeente_.screeningOrganisatie)), cb.equal(r.get(Gemeente_.screeningOrganisatie), screeningOrganisatie));
	}

	public static Specification<Gemeente> isGemeenteActiefOpMoment(Date nu)
	{
		return RangeSpecification.bevat(
			(r, q, cb) -> cb.coalesce(r.get(Gemeente_.beginDatum), DateUtil.BEGIN_OF_TIME),
			(r, q, cb) -> cb.coalesce(r.get(Gemeente_.eindDatum), DateUtil.END_OF_TIME),
			Pair.of(BoundType.CLOSED, BoundType.CLOSED),
			nu
		);
	}

	public static ExtendedSpecification<Gemeente> heeftBmhkLaboratorium(Long bmhkLabId)
	{
		return (r, q, cb) ->
			cb.equal(r.get(Gemeente_.bmhkLaboratorium), bmhkLabId);
	}

	public static ExtendedSpecification<Gemeente> heeftGeenUitnodigingsGebieden()
	{
		return (r, q, cb) -> cb.isEmpty(r.get(Gemeente_.UITNODIGINGS_GEBIEDEN));
	}

	public static ExtendedSpecification<Gemeente> heeftCode(String code)
	{
		return (r, q, cb) -> cb.equal(r.get(Gemeente_.code), code);
	}
}
