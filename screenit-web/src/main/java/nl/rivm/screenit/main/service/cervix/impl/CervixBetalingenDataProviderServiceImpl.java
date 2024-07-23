package nl.rivm.screenit.main.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Root;

import nl.rivm.screenit.main.service.RepositoryDataProviderService;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht_;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.repository.cervix.CervixBetaalopdrachtRepository;
import nl.rivm.screenit.specification.cervix.CervixBetaalopdrachtSpecification;

import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@Service("cervixBetalingenDataProviderService")
public class CervixBetalingenDataProviderServiceImpl extends RepositoryDataProviderService<CervixBetaalopdracht, CervixBetaalopdrachtRepository, Void>
{
	@Override
	protected Specification<CervixBetaalopdracht> getSpecification(Void filter, Sort sort)
	{
		return CervixBetaalopdrachtSpecification.heeftNietStatus(BestandStatus.VERWIJDERD);
	}

	@Override
	protected Order addJoinsForSortingOrCreateDedicatedOrders(Sort.Order order, Root<CervixBetaalopdracht> r, CriteriaBuilder cb)
	{
		if (order.getProperty().startsWith(CervixBetaalopdracht_.SCREENING_ORGANISATIE))
		{
			join(r, CervixBetaalopdracht_.screeningOrganisatie);
		}
		return super.addJoinsForSortingOrCreateDedicatedOrders(order, r, cb);
	}
}
