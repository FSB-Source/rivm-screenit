package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.time.LocalDateTime;
import java.time.temporal.TemporalAdjusters;
import java.util.List;

import javax.persistence.criteria.Root;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaConclusieReviewZoekObject;
import nl.rivm.screenit.main.service.RepositoryDataProviderService;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.mamma.MammaConclusieReview;
import nl.rivm.screenit.model.mamma.MammaConclusieReview_;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.enums.MammaFollowUpConclusieStatus;
import nl.rivm.screenit.repository.mamma.MammaConclusieReviewRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.mamma.MammaConclusieReviewSpecification.filterOpFollowUpStatusGewijzigdOpOfNaMoment;
import static nl.rivm.screenit.specification.mamma.MammaConclusieReviewSpecification.heeftFollowUpConclusieStatus;
import static nl.rivm.screenit.specification.mamma.MammaConclusieReviewSpecification.heeftGeenConclusieReviewMetRetourCeRedenInRonde;
import static nl.rivm.screenit.specification.mamma.MammaConclusieReviewSpecification.heeftNietReviewAlsCoordinerendRadioloog;
import static nl.rivm.screenit.specification.mamma.MammaConclusieReviewSpecification.heeftRadioloog;
import static nl.rivm.screenit.specification.mamma.MammaConclusieReviewSpecification.heeftRadioloogEnNietVerwijzendeLezing;
import static nl.rivm.screenit.specification.mamma.MammaConclusieReviewSpecification.heeftReviewMomentOpOfNaDatum;
import static nl.rivm.screenit.specification.mamma.MammaConclusieReviewSpecification.heeftRondeGereviewedAlsCoordinerendRadioloog;
import static nl.rivm.screenit.specification.mamma.MammaConclusieReviewSpecification.heeftSignalerenMetAfwijking;
import static nl.rivm.screenit.specification.mamma.MammaConclusieReviewSpecification.isNietGereviewed;

@Slf4j
@Service("mammaConclusieReviewDataProviderService")
public class MammaConclusieReviewDataProviderServiceImpl extends RepositoryDataProviderService<MammaConclusieReview, MammaConclusieReviewRepository, MammaConclusieReviewZoekObject>
{
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private SessionFactory sessionFactory;

	private static final int MAMMA_AANTAL_JAAR_REVIEWS_TERUGZIEN = 1;

	private static final String LAATSTE_ONDERZOEK_PROPERTY = "screeningRonde.laatsteOnderzoek.";

	private static final String PERSOON_PROPERTY = "screeningRonde.dossier.client.persoon.";

	@Autowired
	private MammaConclusieReviewRepository conclusieReviewRepository;

	@Override
	protected Specification<MammaConclusieReview> getSpecification(MammaConclusieReviewZoekObject filter, Sort sortParam)
	{
		var tonenNaDatum = bepaalDatumTonenVanuitZoekobject(filter);

		return heeftGeenDuplicatenConclusieReview(
			(filter.isGezienTonen() ? heeftReviewMomentOpOfNaDatum(tonenNaDatum) : isNietGereviewed())
				.and(filterOpFollowUpStatusGewijzigdOpOfNaMoment(filter.getZoekenVanafEindconclusieDatum()))
				.and(heeftRadioloog(filter.getRadioloog()))
				.and(heeftNietReviewAlsCoordinerendRadioloog())
				.and(getSpecificationBijReviewFilterOptie(filter))
				.and(bepaalEnMaakCoordinerendRadioloogSpecification(filter))
		).and(heeftGeenConclusieReviewMetRetourCeRedenInRonde()).and(maakJoinsSpecificationForSort(sortParam));
	}

	public List<Long> zoekBeoordelingIdsMetConclusie(MammaConclusieReviewZoekObject zoekObject, Sort sort)
	{
		return conclusieReviewRepository.findWith(heeftBeoordelingMetConclusie(zoekObject, sort), Long.class,
			q -> q.projection((cb, r) ->
			{
				var beoordelingJoin = join(join(join(r, MammaConclusieReview_.screeningRonde), MammaScreeningRonde_.laatsteOnderzoek), MammaOnderzoek_.laatsteBeoordeling);
				return beoordelingJoin.get(AbstractHibernateObject_.id);
			}).sortBy(sort).all());
	}

	private Specification<MammaConclusieReview> heeftBeoordelingMetConclusie(MammaConclusieReviewZoekObject zoekObject, Sort sort)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var subqueryRoot = subquery.from(MammaConclusieReview.class);
			var baseWerklijstSpecification = getSpecification(zoekObject, sort);

			subquery.select(subqueryRoot.get(AbstractHibernateObject_.id)).where(baseWerklijstSpecification.toPredicate(subqueryRoot, q, cb));
			return cb.in(r.get(AbstractHibernateObject_.id)).value(subquery);
		};
	}

	private LocalDateTime bepaalDatumTonenVanuitZoekobject(MammaConclusieReviewZoekObject filter)
	{
		var vandaag = currentDateSupplier.getLocalDateTime();
		var beginKalenderJaar = vandaag.with(TemporalAdjusters.firstDayOfYear()).toLocalDate().atStartOfDay();
		return filter.isVoorDashboard() ? beginKalenderJaar : vandaag.minusYears(MAMMA_AANTAL_JAAR_REVIEWS_TERUGZIEN);
	}

	private Specification<MammaConclusieReview> maakJoinsSpecificationForSort(Sort sortParam)
	{
		return (r, q, cb) ->
		{
			sortParam.stream().forEach(order -> voegJoinsToeVoorProperty(r, order.getProperty()));
			return null;
		};
	}

	private void voegJoinsToeVoorProperty(Root<MammaConclusieReview> r, String sortProperty)
	{
		if (sortProperty.startsWith(LAATSTE_ONDERZOEK_PROPERTY))
		{
			var rondeJoin = join(r, MammaConclusieReview_.screeningRonde);
			join(rondeJoin, MammaScreeningRonde_.laatsteOnderzoek);
		}
		else if (sortProperty.startsWith(PERSOON_PROPERTY))
		{
			var rondeJoin = join(r, MammaConclusieReview_.screeningRonde);
			var dossierJoin = join(rondeJoin, MammaScreeningRonde_.dossier);
			var clientJoin = join(dossierJoin, MammaDossier_.client);
			join(clientJoin, Client_.persoon);
		}
	}

	private static Specification<MammaConclusieReview> getSpecificationBijReviewFilterOptie(MammaConclusieReviewZoekObject filter)
	{
		return switch (filter.getFilterOptie())
		{
			case FALSE_NEGATIVE_MBB_SIGNALERING -> heeftSignalerenMetAfwijking()
				.and(heeftFollowUpConclusieStatus(MammaFollowUpConclusieStatus.FALSE_NEGATIVE));
			case FALSE_NEGATIVE -> heeftFollowUpConclusieStatus(MammaFollowUpConclusieStatus.FALSE_NEGATIVE);
			case FALSE_POSITIVE -> heeftFollowUpConclusieStatus(MammaFollowUpConclusieStatus.FALSE_POSITIVE);
			case TRUE_POSITIVE -> heeftFollowUpConclusieStatus(MammaFollowUpConclusieStatus.TRUE_POSITIVE);
			case TRUE_POSITIVE_INDIVIDU_GEMIST -> heeftFollowUpConclusieStatus(MammaFollowUpConclusieStatus.TRUE_POSITIVE).and(
				heeftRadioloogEnNietVerwijzendeLezing(filter.getRadioloog()));
			default -> (r, q, cb) -> null;
		};
	}

	private static Specification<MammaConclusieReview> bepaalEnMaakCoordinerendRadioloogSpecification(MammaConclusieReviewZoekObject filter)
	{
		if (filter.getCoordinerendRadioloogKijktBijAndereRadioloog())
		{
			if (filter.isGezienCoordinerendRadioloogTonen())
			{
				return heeftRondeGereviewedAlsCoordinerendRadioloog(filter.getIngelogdeGebruiker());
			}
			return (r, q, cb) ->
				cb.not(heeftRondeGereviewedAlsCoordinerendRadioloog(filter.getIngelogdeGebruiker()).toPredicate(r, q, cb));
		}
		return (r, q, cb) -> null;
	}

	private static Specification<MammaConclusieReview> heeftGeenDuplicatenConclusieReview(Specification<MammaConclusieReview> specification)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var subqueryRoot = subquery.from(MammaConclusieReview.class);
			subquery.select(subqueryRoot.get(AbstractHibernateObject_.id)).where(specification.toPredicate(subqueryRoot, q, cb));

			return cb.in(r.get(AbstractHibernateObject_.id)).value(subquery);
		};
	}

}
