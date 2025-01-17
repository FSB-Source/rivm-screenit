package nl.rivm.screenit.batch.repository;

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

import java.util.List;

import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;

import nl.rivm.screenit.model.ClientBrief_;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode_;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.repository.BaseJpaRepository;
import nl.rivm.screenit.specification.algemeen.ClientBriefSpecification;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.domain.Specification;

import static javax.persistence.criteria.JoinType.LEFT;
import static nl.rivm.screenit.specification.ExtendedSpecification.not;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.BriefSpecification.heeftBriefType;
import static nl.rivm.screenit.specification.algemeen.ClientBriefSpecification.heeftScreeningsOrganisatieId;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftIndicatie;
import static nl.rivm.screenit.specification.algemeen.MammaBriefSpecification.clientHeeftAfspraak;
import static nl.rivm.screenit.specification.algemeen.MammaBriefSpecification.laatsteUitnodigingJoin;

public interface MammaBriefRepository extends BaseJpaRepository<MammaBrief>
{
	default List<Long> getAfspraakStandplaatsenIdsMetBrief(ScreeningOrganisatie screeningOrganisatie, BriefType briefType)
	{
		var specification = clientHeeftAfspraak().and(moetGegenereerdWorden(screeningOrganisatie, briefType));
		return findWith(specification, Long.class,
			q -> q.distinct()
				.projection((cb, r) -> afspraakStandplaatsRondeJoin(laatsteAfspraakJoin(r)).get(MammaStandplaatsRonde_.standplaats).get(AbstractHibernateObject_.id))
				.all());
	}

	default List<Long> getUitnodigingStandplaatsenIdsMetBrief(ScreeningOrganisatie screeningOrganisatie, BriefType briefType)
	{
		var specification = not(clientHeeftAfspraak()).and(moetGegenereerdWorden(screeningOrganisatie, briefType));
		return findWith(specification, Long.class,
			q -> q.distinct()
				.projection((cb, r) -> uitnodigingStandplaatsRondeJoin(laatsteUitnodigingJoin(r)).get(MammaStandplaatsRonde_.standplaats).get(AbstractHibernateObject_.id))
				.all());
	}

	private static Specification<MammaBrief> moetGegenereerdWorden(ScreeningOrganisatie screeningOrganisatie, BriefType briefType)
	{
		return ClientBriefSpecification.<MammaBrief> magGegenereerdWorden()
			.and(heeftBriefType(briefType))
			.and(heeftIndicatie().with(r -> join(r, ClientBrief_.client)))
			.and(heeftScreeningsOrganisatieId(screeningOrganisatie.getId()));
	}

	private static Join<MammaUitnodiging, MammaAfspraak> laatsteAfspraakJoin(From<?, ? extends MammaBrief> r)
	{
		var laatsteUitnodigingJoin = laatsteUitnodigingJoin(r);
		return join(laatsteUitnodigingJoin, MammaUitnodiging_.laatsteAfspraak, LEFT);
	}

	private static Join<MammaStandplaatsPeriode, MammaStandplaatsRonde> afspraakStandplaatsRondeJoin(Join<?, MammaAfspraak> afspraakJoin)
	{
		var standplaatsPeriodeJoin = join(afspraakJoin, MammaAfspraak_.standplaatsPeriode);
		return join(standplaatsPeriodeJoin, MammaStandplaatsPeriode_.standplaatsRonde);
	}

	private static Join<MammaUitnodiging, MammaStandplaatsRonde> uitnodigingStandplaatsRondeJoin(Join<?, MammaUitnodiging> uitnodigingJoin)
	{
		return join(uitnodigingJoin, MammaUitnodiging_.standplaatsRonde);
	}

}
