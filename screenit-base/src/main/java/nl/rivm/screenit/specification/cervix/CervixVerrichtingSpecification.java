package nl.rivm.screenit.specification.cervix;

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
import java.util.ArrayList;
import java.util.List;

import javax.persistence.criteria.Predicate;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.ScreeningOrganisatie_;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.enums.CervixTariefType;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.model.cervix.facturatie.CervixVerrichting;
import nl.rivm.screenit.model.cervix.facturatie.CervixVerrichting_;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.functionalinterfaces.PathAwarePredicate;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.composePredicates;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNull;
import static nl.rivm.screenit.specification.cervix.CervixBoekRegelSpecification.verrichtingJoin;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixVerrichtingSpecification
{

	public static Specification<CervixBoekRegel> heeftNietVerrichtingType(CervixTariefType verrichtingType)
	{
		return (r, q, cb) -> cb.notEqual(verrichtingJoin(r).get(CervixVerrichting_.type), verrichtingType);
	}

	public static Specification<CervixBoekRegel> filterVerrichtingType(CervixTariefType type)
	{
		return skipWhenNull(type, (r, q, cb) -> cb.equal(verrichtingJoin(r).get(CervixVerrichting_.type), type));
	}

	public static Specification<CervixVerrichting> filterVerrichtingTypeVoorVerrichting(CervixTariefType type)
	{
		return skipWhenNull(type, (r, q, cb) -> cb.equal(r.get(CervixVerrichting_.type), type));
	}

	public static Specification<CervixVerrichting> filterVerrichtingType(boolean huisartsVerichtingType, boolean labTariefTypes)
	{
		return (r, q, cb) ->
		{
			var values = new ArrayList<CervixTariefType>();
			if (huisartsVerichtingType && !labTariefTypes)
			{
				values.addAll(CervixTariefType.getAlleHuisartsTariefTypes());
			}
			else if (labTariefTypes && !huisartsVerichtingType)
			{
				values.addAll(CervixTariefType.getAlleLabTariefTypes());
			}
			else
			{
				values.addAll(List.of(CervixTariefType.values()));
			}
			return r.get(CervixVerrichting_.type).in(values);
		};
	}

	public static Specification<CervixBoekRegel> filterHuisartsLocatie(CervixHuisartsLocatie huisartsLocatie)
	{
		return skipWhenNull(huisartsLocatie, (r, q, cb) -> cb.equal(verrichtingJoin(r).get(CervixVerrichting_.huisartsLocatie), huisartsLocatie));
	}

	public static Specification<CervixBoekRegel> verichtingsDatumValtTussenVoorBoekRegel(LocalDate vanafDatum, LocalDate totEnMetDatum)
	{
		return (r, q, cb) -> verichtingsDatumValtTussenPredicate(vanafDatum, totEnMetDatum).withPath(cb, verrichtingJoin(r));
	}

	public static Specification<CervixVerrichting> verichtingsDatumValtTussenVoorVerrichting(LocalDate vanafDatum, LocalDate totEnMetDatum)
	{
		return (r, q, cb) -> verichtingsDatumValtTussenPredicate(vanafDatum, totEnMetDatum).withPath(cb, r);
	}

	public static Specification<CervixVerrichting> isKleinerDanVerrichtingsDatumVoorVerrichting(LocalDate totEnMetDatum)
	{
		return skipWhenNull(totEnMetDatum, (r, q, cb) -> isKleinerDanVerichtingsDatumPredicate(totEnMetDatum).withPath(cb, r));
	}

	public static Specification<CervixBoekRegel> filterRegio(ScreeningOrganisatie screeningOrganisatie)
	{
		return skipWhenNull(screeningOrganisatie,
			(r, q, cb) -> cb.equal(verrichtingJoin(r).get(CervixVerrichting_.regio), screeningOrganisatie));
	}

	public static Specification<CervixVerrichting> filterVerrichtingBinnenRegio(Long screeningOrganisatieId)
	{
		return skipWhenNull(screeningOrganisatieId, (r, q, cb) ->
		{
			var regioPath = join(r, CervixVerrichting_.regio);
			var regioId = regioPath.get(ScreeningOrganisatie_.id);

			return cb.equal(regioId, screeningOrganisatieId);
		});
	}

	public static Specification<CervixVerrichting> heeftTypeIn(List<CervixTariefType> types)
	{
		return (r, q, cb) -> r.get(CervixVerrichting_.type).in(types);
	}

	private static PathAwarePredicate<CervixVerrichting> verichtingsDatumValtTussenPredicate(LocalDate vanafDatum, LocalDate totEnMetDatum)
	{
		return (cb, r) ->
		{
			var predicates = new ArrayList<Predicate>();
			if (vanafDatum != null)
			{
				predicates.add(cb.greaterThanOrEqualTo(r.get(CervixVerrichting_.verrichtingsDatum), DateUtil.toUtilDate(vanafDatum)));
			}
			if (totEnMetDatum != null)
			{
				predicates.add(isKleinerDanVerichtingsDatumPredicate(totEnMetDatum).withPath(cb, r));
			}
			return composePredicates(cb, predicates);
		};
	}

	private static PathAwarePredicate<CervixVerrichting> isKleinerDanVerichtingsDatumPredicate(LocalDate totEnMetDatum)
	{
		return (cb, r) -> cb.lessThan(r.get(CervixVerrichting_.verrichtingsDatum), DateUtil.toUtilDate(totEnMetDatum.plusDays(1)));
	}
}
