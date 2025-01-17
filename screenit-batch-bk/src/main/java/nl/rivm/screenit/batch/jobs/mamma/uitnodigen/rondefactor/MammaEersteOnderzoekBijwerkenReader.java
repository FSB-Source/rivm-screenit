package nl.rivm.screenit.batch.jobs.mamma.uitnodigen.rondefactor;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.Root;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaMammografie;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode_;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde_;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.ExtendedSpecification.not;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.mamma.MammaBaseDossierSpecification.heeftEersteOnderzoek;
import static nl.rivm.screenit.specification.mamma.MammaMammografieBaseSpecification.heeftIlmStatus;
import static nl.rivm.screenit.specification.mamma.MammaStandplaatsPeriodeSpecification.eindigtOpOfNaDatum;

@Component
@AllArgsConstructor
public class MammaEersteOnderzoekBijwerkenReader extends BaseSpecificationScrollableResultReader<MammaDossier>
{
	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	protected Specification<MammaDossier> createSpecification()
	{
		return heeftEersteOnderzoek().and(heeftNietIlmStatusNietBeschikbaar().and(heeftGeenLopendeStandplaatsRonde()));
	}

	private Specification<MammaDossier> heeftNietIlmStatusNietBeschikbaar()
	{
		return not(heeftIlmStatus(MammaMammografieIlmStatus.NIET_BESCHIKBAAR).with(r -> mammografieJoin(r)));
	}

	private static Join<MammaOnderzoek, MammaMammografie> mammografieJoin(From<?, ? extends MammaDossier> dossierRoot)
	{
		var screeningRondeJoin = join(dossierRoot, MammaDossier_.screeningRondes);
		var laatsteOnderzoekJoin = join(screeningRondeJoin, MammaScreeningRonde_.laatsteOnderzoek);
		return join(laatsteOnderzoekJoin, MammaOnderzoek_.mammografie);
	}

	private Specification<MammaDossier> heeftGeenLopendeStandplaatsRonde()
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var subqueryRoot = subquery.from(MammaStandplaatsRonde.class);

			var subStandplaatsPeriodenJoin = join(subqueryRoot, MammaStandplaatsRonde_.standplaatsPerioden);

			subquery.select(subqueryRoot.get(AbstractHibernateObject_.id))
				.where(eindigtOpOfNaDatum(currentDateSupplier.getLocalDate()).toPredicate(subStandplaatsPeriodenJoin, q, cb));

			return cb.not(standplaatsRondeJoin(r).get(AbstractHibernateObject_.id).in(subquery));
		};
	}

	private static Join<MammaStandplaatsPeriode, MammaStandplaatsRonde> standplaatsRondeJoin(Root<MammaDossier> dossierRoot)
	{
		var screeningRondeJoin = join(dossierRoot, MammaDossier_.screeningRondes);
		var laatsteOnderZoekJoin = join(screeningRondeJoin, MammaScreeningRonde_.laatsteOnderzoek);
		var afspraakJoin = join(laatsteOnderZoekJoin, MammaOnderzoek_.afspraak);
		var standplaatsPeriodeJoin = join(afspraakJoin, MammaAfspraak_.standplaatsPeriode);
		return join(standplaatsPeriodeJoin, MammaStandplaatsPeriode_.standplaatsRonde);
	}
}
