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

import java.time.LocalDateTime;
import java.util.Date;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.From;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Predicate;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaBeoordeling_;
import nl.rivm.screenit.model.mamma.MammaConclusieReview;
import nl.rivm.screenit.model.mamma.MammaConclusieReview_;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.MammaLezing_;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaSignaleren_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.model.mamma.enums.MammaBIRADSWaarde;
import nl.rivm.screenit.model.mamma.enums.MammaFollowUpConclusieStatus;
import nl.rivm.screenit.specification.SpecificationUtil;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNull;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaConclusieReviewSpecification
{

	public static Specification<MammaConclusieReview> heeftReviewMomentOpOfNaDatum(LocalDateTime datum)
	{
		return (r, q, cb) -> cb.greaterThanOrEqualTo(r.get(MammaConclusieReview_.reviewMoment), datum);
	}

	public static Specification<MammaConclusieReview> isNietGereviewed()
	{
		return (r, q, cb) -> cb.isNull(r.get(MammaConclusieReview_.reviewMoment));
	}

	public static Specification<MammaConclusieReview> filterOpFollowUpStatusGewijzigdOpOfNaMoment(Date datum)
	{
		return skipWhenNull(datum,
			(r, q, cb) ->
			{
				var rondeJoin = SpecificationUtil.join(r, MammaConclusieReview_.screeningRonde);
				return cb.greaterThanOrEqualTo(rondeJoin.get(MammaScreeningRonde_.followUpConclusieStatusGewijzigdOp), datum);
			});
	}

	public static Specification<MammaConclusieReview> heeftRadioloog(InstellingGebruiker radioloog)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaConclusieReview_.radioloog), radioloog);
	}

	public static Specification<MammaConclusieReview> heeftNietReviewAlsCoordinerendRadioloog()
	{
		return (r, q, cb) -> cb.isFalse(r.get(MammaConclusieReview_.reviewAlsCoordinerendRadioloog));
	}

	public static Specification<MammaConclusieReview> heeftSignalerenMetAfwijking()
	{
		return (r, q, cb) ->
		{
			var rondeJoin = SpecificationUtil.join(r, MammaConclusieReview_.screeningRonde);
			var uitnodigingJoin = SpecificationUtil.join(rondeJoin, MammaScreeningRonde_.uitnodigingen);
			var afspraakJoin = SpecificationUtil.join(uitnodigingJoin, MammaUitnodiging_.afspraken);
			var onderzoekJoin = SpecificationUtil.join(afspraakJoin, MammaAfspraak_.onderzoek);
			var signalerenJoin = SpecificationUtil.join(onderzoekJoin, MammaOnderzoek_.signaleren);
			return cb.isTrue(signalerenJoin.get(MammaSignaleren_.heeftAfwijkingen));
		};
	}

	public static Specification<MammaConclusieReview> heeftFollowUpConclusieStatus(MammaFollowUpConclusieStatus conclusieStatus)
	{
		return (r, q, cb) ->
		{
			var rondeJoin = SpecificationUtil.join(r, MammaConclusieReview_.screeningRonde);
			return cb.equal(rondeJoin.get(MammaScreeningRonde_.followUpConclusieStatus), conclusieStatus);
		};
	}

	public static Specification<MammaConclusieReview> heeftRadioloogEnNietVerwijzendeLezing(InstellingGebruiker radioloog)
	{
		return (r, q, cb) ->
		{
			var rondeJoin = SpecificationUtil.join(r, MammaConclusieReview_.screeningRonde);
			var uitnodigingJoin = SpecificationUtil.join(rondeJoin, MammaScreeningRonde_.uitnodigingen);
			var afspraakJoin = SpecificationUtil.join(uitnodigingJoin, MammaUitnodiging_.afspraken);
			var onderzoekJoin = SpecificationUtil.join(afspraakJoin, MammaAfspraak_.onderzoek);
			var beoordelingenJoin = SpecificationUtil.join(onderzoekJoin, MammaOnderzoek_.beoordelingen);
			var eersteLezingJoin = SpecificationUtil.join(beoordelingenJoin, MammaBeoordeling_.eersteLezing, JoinType.LEFT);
			var tweedeLezingJoin = SpecificationUtil.join(beoordelingenJoin, MammaBeoordeling_.tweedeLezing, JoinType.LEFT);
			var arbitrageLezingJoin = SpecificationUtil.join(beoordelingenJoin, MammaBeoordeling_.arbitrageLezing, JoinType.LEFT);
			return cb.or(
				heeftRadioloogEnNietVerwijzendeLezingPredicate(radioloog, cb, eersteLezingJoin),
				heeftRadioloogEnNietVerwijzendeLezingPredicate(radioloog, cb, tweedeLezingJoin),
				heeftRadioloogEnNietVerwijzendeLezingPredicate(radioloog, cb, arbitrageLezingJoin)
			);
		};
	}

	private static Predicate heeftRadioloogEnNietVerwijzendeLezingPredicate(InstellingGebruiker radioloog, CriteriaBuilder cb, From<MammaBeoordeling, MammaLezing> from)
	{
		return cb.and(
			from.get(MammaLezing_.biradsLinks).in(MammaBIRADSWaarde.getNietVerwijzendBIRADSWaarden()),
			from.get(MammaLezing_.biradsRechts).in(MammaBIRADSWaarde.getNietVerwijzendBIRADSWaarden()),
			cb.equal(from.get(MammaLezing_.beoordelaar), radioloog)
		);
	}

	public static Specification<MammaConclusieReview> heeftRondeGereviewedAlsCoordinerendRadioloog(InstellingGebruiker coordinerendRadioloog)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var subqueryRoot = subquery.from(MammaConclusieReview.class);

			subquery.select(subqueryRoot.get(AbstractHibernateObject_.id)).where(cb.and(
				cb.equal(subqueryRoot.get(MammaConclusieReview_.screeningRonde), r.get(MammaConclusieReview_.screeningRonde)),
				cb.isTrue(subqueryRoot.get(MammaConclusieReview_.reviewAlsCoordinerendRadioloog)),
				cb.equal(subqueryRoot.get(MammaConclusieReview_.radioloog), coordinerendRadioloog),
				cb.isNotNull(subqueryRoot.get(MammaConclusieReview_.reviewMoment))
			));
			return cb.exists(subquery);
		};
	}

	public static Specification<MammaConclusieReview> heeftGeenConclusieReviewMetRetourCeRedenInRonde()
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var subqueryRoot = subquery.from(MammaConclusieReview.class);
			var alleConclusieReviewsJoin = subqueryRoot.join(MammaConclusieReview_.screeningRonde).join(MammaScreeningRonde_.conclusieReviews);

			subquery.select(subqueryRoot.get(AbstractHibernateObject_.id)).where(cb.isNotNull(alleConclusieReviewsJoin.get(MammaConclusieReview_.retourCeReden)));

			return cb.not(cb.in(r.get(AbstractHibernateObject_.id)).value(subquery));
		};
	}

}
