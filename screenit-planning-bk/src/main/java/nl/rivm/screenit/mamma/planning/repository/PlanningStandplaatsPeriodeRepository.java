package nl.rivm.screenit.mamma.planning.repository;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.persistence.Tuple;
import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.Root;

import nl.rivm.screenit.mamma.planning.repository.projectie.StandplaatsPeriodeProjectie;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.ScreeningOrganisatie_;
import nl.rivm.screenit.model.SingleTableHibernateObject_;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode_;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde_;
import nl.rivm.screenit.model.mamma.MammaStandplaats_;
import nl.rivm.screenit.repository.BaseJpaRepository;
import nl.rivm.screenit.specification.mamma.MammaScreeningsEenheidSpecification;
import nl.rivm.screenit.specification.mamma.MammaStandplaatsPeriodeSpecification;
import nl.rivm.screenit.specification.mamma.MammaStandplaatsSpecification;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.mamma.MammaStandplaatsPeriodeSpecification.heeftScreeningsEenheidId;

public interface PlanningStandplaatsPeriodeRepository extends BaseJpaRepository<MammaStandplaatsPeriode>
{
	default Map<Long, Integer> findVolgnummersEersteActieveStandplaatsPeriodePerSe(LocalDate vandaag)
	{
		var specs = MammaStandplaatsSpecification.isActief().withRoot(this::standplaatsJoin)

			.and(MammaStandplaatsPeriodeSpecification.eindigtOpOfNaDatum(vandaag).withRoot(this::allePeriodenBinnenRondeJoin))
			.and(MammaScreeningsEenheidSpecification.isActief().withRoot(r -> screeningsEenheidJoin(allePeriodenBinnenRondeJoin(r))));

		return findWith(specs, Tuple.class, q -> q
			.groupBy((cb, r) -> List.of(r.get(MammaStandplaatsPeriode_.screeningsEenheid)))
			.projections((cb, r) ->
				List.of(
					r.get(MammaStandplaatsPeriode_.screeningsEenheid).get(AbstractHibernateObject_.id),
					cb.min(r.get(MammaStandplaatsPeriode_.screeningsEenheidVolgNr))
				)
			).all()
		).stream().collect(
			Collectors.toMap(
				tuple -> tuple.get(0, Long.class),
				tuple -> tuple.get(1, Integer.class)
			)
		);
	}

	default List<StandplaatsPeriodeProjectie> findStandplaatsPeriodesVanafVolgnummer(Map<Long, Integer> eersteVolgnummerPerSe)
	{
		Specification<MammaStandplaatsPeriode> specs = null;

		for (var entry : eersteVolgnummerPerSe.entrySet())
		{
			var spec = heeftScreeningsEenheidId(entry.getKey()).and(MammaStandplaatsPeriodeSpecification.heeftScreeningsEenheidVolgNrVanaf(entry.getValue()));
			specs = specs == null ? spec : specs.or(spec);
		}

		return findWith(specs, StandplaatsPeriodeProjectie.class, q -> q
			.projections((cb, r) ->
				List.of(
					r.get(AbstractHibernateObject_.id),
					r.get(MammaStandplaatsPeriode_.standplaatsRonde).get(AbstractHibernateObject_.id),
					screeningOrganisatieJoin(r).get(SingleTableHibernateObject_.id),
					screeningOrganisatieJoin(r).get(ScreeningOrganisatie_.wekenVanTevorenUitnodigen),
					r.get(MammaStandplaatsPeriode_.vanaf).as(LocalDate.class),
					r.get(MammaStandplaatsPeriode_.totEnMet).as(LocalDate.class)
				)
			).all()
		);
	}

	private Join<?, MammaScreeningsEenheid> screeningsEenheidJoin(From<?, ? extends MammaStandplaatsPeriode> r)
	{
		return join(r, MammaStandplaatsPeriode_.screeningsEenheid);
	}

	private Join<?, MammaStandplaatsPeriode> allePeriodenBinnenRondeJoin(Root<MammaStandplaatsPeriode> r)
	{
		var standplaatsRondeJoin = standplaatsRondeJoin(r);
		return join(standplaatsRondeJoin, MammaStandplaatsRonde_.standplaatsPerioden);
	}

	private Join<?, MammaStandplaats> standplaatsJoin(Root<MammaStandplaatsPeriode> r)
	{
		var standplaatsRondeJoin = standplaatsRondeJoin(r);
		return join(standplaatsRondeJoin, MammaStandplaatsRonde_.standplaats);
	}

	private Join<?, ScreeningOrganisatie> screeningOrganisatieJoin(Root<MammaStandplaatsPeriode> r)
	{
		var standplaatsJoin = standplaatsJoin(r);
		return join(standplaatsJoin, MammaStandplaats_.regio);
	}

	private Join<?, MammaStandplaatsRonde> standplaatsRondeJoin(Root<MammaStandplaatsPeriode> r)
	{
		return join(r, MammaStandplaatsPeriode_.standplaatsRonde);
	}
}
