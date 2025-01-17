package nl.rivm.screenit.batch.jobs.cervix.brieven.regio.labformulierenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import java.util.function.Function;

import javax.persistence.criteria.From;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.huisartsenportaal.enums.CervixLocatieStatus;
import nl.rivm.screenit.model.BagAdres_;
import nl.rivm.screenit.model.Gemeente_;
import nl.rivm.screenit.model.SingleTableHibernateObject_;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie_;
import nl.rivm.screenit.model.cervix.CervixLabformulierAanvraag;
import nl.rivm.screenit.model.cervix.CervixLabformulierAanvraag_;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierAanvraagStatus;
import nl.rivm.screenit.specification.cervix.CervixHuisartsLocatieSpecification;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.cervix.CervixHuisartsSpecification.isActief;
import static nl.rivm.screenit.specification.cervix.CervixLabformulierAanvraagSpecification.heeftStatus;

@Component
public class LabformulierGenererenReader extends BaseSpecificationScrollableResultReader<CervixLabformulierAanvraag>
{
	private Long getScreeningOrganisatieId()
	{
		return getStepExecutionContext().getLong(LabformulierGenererenPartitioner.KEY_SCREENINGORGANISATIEID);
	}

	@Override
	protected Specification<CervixLabformulierAanvraag> createSpecification()
	{
		return (r, q, cb) ->
		{
			var screeningOrganisatieId = getScreeningOrganisatieId();

			var subquery = q.subquery(Long.class);
			var subqueryRoot = subquery.from(CervixHuisartsLocatie.class);
			var locatieAdresJoin = subqueryRoot.join(CervixHuisartsLocatie_.locatieAdres);
			var gemeenteJoin = locatieAdresJoin.join(BagAdres_.gbaGemeente);
			var screeningOrganisatieJoin = gemeenteJoin.join(Gemeente_.screeningOrganisatie);

			subquery.select(subqueryRoot.get(AbstractHibernateObject_.id))
				.where(cb.equal(screeningOrganisatieJoin.get(SingleTableHibernateObject_.id), screeningOrganisatieId));

			return cb.and(
				heeftStatus(CervixLabformulierAanvraagStatus.AANGEVRAAGD)
					.and(isActief().with(huisartsJoin())
						.and(CervixHuisartsLocatieSpecification.heeftNietStatus(CervixLocatieStatus.INACTIEF).with(huisartsLocatieJoin())))
					.toPredicate(r, q, cb),
				cb.in(huisartsLocatieJoin().apply(r).get(AbstractHibernateObject_.id)).value(subquery)
			);
		};
	}

	private static Function<From<?, ? extends CervixLabformulierAanvraag>, From<?, ? extends CervixHuisartsLocatie>> huisartsLocatieJoin()
	{
		return q -> join(q, CervixLabformulierAanvraag_.huisartsLocatie);
	}

	private static Function<From<?, ? extends CervixLabformulierAanvraag>, From<?, ? extends CervixHuisarts>> huisartsJoin()
	{
		return q ->
		{
			var huisartsLocatieJoin = huisartsLocatieJoin().apply(q);
			return join(huisartsLocatieJoin, CervixHuisartsLocatie_.huisarts);
		};
	}

}
