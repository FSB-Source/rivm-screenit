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
import java.util.Date;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.SingleTableHibernateObject_;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel_;
import nl.rivm.screenit.model.cervix.facturatie.CervixHuisartsTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixLabTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixLabTarief_;
import nl.rivm.screenit.model.cervix.facturatie.CervixTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixTarief_;
import nl.rivm.screenit.model.cervix.facturatie.CervixVerrichting;
import nl.rivm.screenit.model.cervix.facturatie.CervixVerrichting_;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.functionalinterfaces.PathAwarePredicate;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.composePredicates;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNull;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixTariefSpecification
{

	public static Specification<CervixLabTarief> isLabTariefActief()
	{
		return (r, q, cb) -> cb.isTrue(r.get(CervixTarief_.actief));
	}

	public static Specification<CervixHuisartsTarief> isHuisartsTariefActief()
	{
		return (r, q, cb) -> cb.isTrue(r.get(CervixTarief_.actief));
	}

	public static Specification<CervixBoekRegel> filterLabVoorBoekRegel(BMHKLaboratorium laboratorium)
	{
		return skipWhenNull(laboratorium, (r, q, cb) -> heeftLabPredicate(laboratorium).withPath(cb, cb.treat(join(r, CervixBoekRegel_.tarief), CervixLabTarief.class)));
	}

	public static Specification<CervixLabTarief> heeftLabVoorTarief(BMHKLaboratorium laboratorium)
	{
		return (r, q, cb) -> heeftLabPredicate(laboratorium).withPath(cb, r);
	}

	public static Specification<CervixLabTarief> heeftGeldigVanafdatumVoorLab(Date datum)
	{
		return (r, q, cb) -> cb.equal(r.get(CervixTarief_.geldigVanafDatum), datum);
	}

	public static Specification<CervixHuisartsTarief> heeftGeldigVanafdatumVoorHa(Date datum)
	{
		return (r, q, cb) -> cb.equal(r.get(CervixTarief_.geldigVanafDatum), datum);
	}

	public static Specification<CervixHuisartsTarief> isGeldigHaTariefVoor(LocalDate datum)
	{
		return (r, q, cb) -> checkRange(datum, datum, r, cb);
	}

	public static Specification<CervixLabTarief> isGeldigLabTariefVoor(LocalDate datum)
	{
		return (r, q, cb) -> checkRange(datum, datum, r, cb);
	}

	public static Specification<CervixHuisartsTarief> heeftHaTariefOverlap(LocalDate vanafDatum, LocalDate totEnMetDatum)
	{
		return (r, q, cb) -> checkRange(totEnMetDatum, vanafDatum, r, cb);
	}

	public static Specification<CervixLabTarief> heeftLabTariefOverlap(LocalDate vanafDatum, LocalDate totEnMetDatum)
	{
		return (r, q, cb) -> checkRange(totEnMetDatum, vanafDatum, r, cb);
	}

	public static Specification<CervixLabTarief> isGroterOfGelijkAanGeldigTotEnMetLabTarief(LocalDate totEnMetDatum)
	{
		return (r, q, cb) -> isGroterOfGelijkAanGeldigTotEnMet(totEnMetDatum).withPath(cb, r);
	}

	public static Specification<CervixHuisartsTarief> isGroterOfGelijkAanGeldigTotEnMetHaTarief(LocalDate totEnMetDatum)
	{
		return (r, q, cb) -> isGroterOfGelijkAanGeldigTotEnMet(totEnMetDatum).withPath(cb, r);
	}

	public static Specification<CervixVerrichting> isTariefVoorVerrichting(Long oudTariefId)
	{
		return (r, q, cb) -> cb.equal(join(join(r, CervixVerrichting_.laatsteBoekRegel), CervixBoekRegel_.tarief).get(SingleTableHibernateObject_.id), oudTariefId);
	}

	private static PathAwarePredicate<CervixLabTarief> heeftLabPredicate(BMHKLaboratorium bmhkLaboratorium)
	{
		return (cb, r) -> cb.equal(r.get(CervixLabTarief_.bmhkLaboratorium), bmhkLaboratorium);
	}

	private static Predicate checkRange(LocalDate vanafDatum, LocalDate totEnMetDatum, Root<? extends CervixTarief> r, CriteriaBuilder cb)
	{
		var predicates = new ArrayList<Predicate>();

		if (vanafDatum != null)
		{
			predicates.add(isKleinerOfGelijkAanGeldigVanafDatum(vanafDatum).withPath(cb, r));
		}
		if (totEnMetDatum != null)
		{
			predicates.add(isGroterOfGelijkAanGeldigTotEnMet(totEnMetDatum).withPath(cb, r));
		}
		return composePredicates(cb, predicates);
	}

	private static PathAwarePredicate<CervixTarief> isKleinerOfGelijkAanGeldigVanafDatum(LocalDate vanafDatum)
	{
		return (cb, r) -> cb.lessThanOrEqualTo(r.get(CervixTarief_.geldigVanafDatum), DateUtil.toUtilDate(vanafDatum));
	}

	private static PathAwarePredicate<CervixTarief> isGroterOfGelijkAanGeldigTotEnMet(LocalDate totEnMetDatum)
	{
		return (cb, r) -> cb.greaterThanOrEqualTo(cb.coalesce(r.get(CervixTarief_.geldigTotenmetDatum), DateUtil.parseDateForPattern("01-01-4000", Constants.DEFAULT_DATE_FORMAT)),
			DateUtil.toUtilDate(totEnMetDatum));
	}

}
