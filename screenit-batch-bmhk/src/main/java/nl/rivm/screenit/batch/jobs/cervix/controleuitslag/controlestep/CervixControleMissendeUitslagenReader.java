package nl.rivm.screenit.batch.jobs.cervix.controleuitslag.controlestep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Expression;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Root;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.TablePerClassHibernateObject_;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixMonster_;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde_;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.cervix.CervixBaseMonsterService;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.Constants.MAX_AANTAL_DAGEN_TERUGKIJKEN_CONTROLE_MISSENDE_UITSLAGEN;
import static nl.rivm.screenit.specification.SpecificationUtil.join;

@Component
@AllArgsConstructor
public class CervixControleMissendeUitslagenReader extends BaseSpecificationScrollableResultReader<CervixMonster>
{
	private final CervixBaseMonsterService monsterService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final OrganisatieParameterService organisatieParameterService;

	@Override
	protected Order getOrder(Root<CervixMonster> r, CriteriaBuilder cb)
	{
		return cb.asc(createProjection(r, cb));
	}

	@Override
	protected Specification<CervixMonster> createSpecification()
	{
		var vandaag = currentDateSupplier.getLocalDate();
		var minimaleSignaleringsDatum = vandaag.minusDays(
			organisatieParameterService.getOrganisatieParameter(null, OrganisatieParameterKey.CERVIX_SIGNALERINGSTERMIJN_MISSENDE_UITSLAGEN, 30));
		var signalerenVanaf = vandaag.minusDays(MAX_AANTAL_DAGEN_TERUGKIJKEN_CONTROLE_MISSENDE_UITSLAGEN);
		return monsterService.maakMonsterMetMissendeUitslagSpecification(signalerenVanaf, minimaleSignaleringsDatum);
	}

	@Override
	protected Expression<Long> createProjection(Root<CervixMonster> r, CriteriaBuilder cb)
	{
		return join(join(r, CervixMonster_.ontvangstScreeningRonde), CervixScreeningRonde_.dossier).get(TablePerClassHibernateObject_.id);
	}

	@Override
	protected Class<?> getResultClass()
	{
		return Object[].class;
	}
}
