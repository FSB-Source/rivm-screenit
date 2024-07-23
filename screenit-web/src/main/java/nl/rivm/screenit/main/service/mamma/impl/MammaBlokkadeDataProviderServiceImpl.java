package nl.rivm.screenit.main.service.mamma.impl;

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

import nl.rivm.screenit.main.service.RepositoryDataProviderService;
import nl.rivm.screenit.model.mamma.MammaBlokkade;
import nl.rivm.screenit.repository.mamma.MammaBaseBlokkadeRepository;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.specification.mamma.MammaBaseBlokkadeSpecification.filterOpActief;
import static nl.rivm.screenit.specification.mamma.MammaBaseBlokkadeSpecification.filterOpRegio;
import static nl.rivm.screenit.specification.mamma.MammaBaseBlokkadeSpecification.filterOpScreeningsEenheid;
import static nl.rivm.screenit.specification.mamma.MammaBaseBlokkadeSpecification.filterOpStandplaats;
import static nl.rivm.screenit.specification.mamma.MammaBaseBlokkadeSpecification.filterOpTotEnMetNietVoor;
import static nl.rivm.screenit.specification.mamma.MammaBaseBlokkadeSpecification.filterOpType;
import static nl.rivm.screenit.specification.mamma.MammaBaseBlokkadeSpecification.filterOpVanafNietNa;
import static nl.rivm.screenit.specification.mamma.MammaBaseBlokkadeSpecification.getBlokkades;

@Service("MammaBlokkadeDataProviderService")
public class MammaBlokkadeDataProviderServiceImpl extends RepositoryDataProviderService<MammaBlokkade, MammaBaseBlokkadeRepository, MammaBlokkade>
{
	@Override
	protected Specification<MammaBlokkade> getSpecification(MammaBlokkade filter, Sort sortParam)
	{
		return getBlokkades()
			.and(filterOpActief(filter.getActief()))
			.and(filterOpType(filter.getType()))
			.and(filterOpRegio(filter.getRegio()))
			.and(filterOpStandplaats(filter.getStandplaats()))
			.and(filterOpScreeningsEenheid(filter.getScreeningsEenheid()))
			.and(filterOpVanafNietNa(DateUtil.toLocalDate(filter.getTotEnMet())))
			.and(filterOpTotEnMetNietVoor(DateUtil.toLocalDate(filter.getVanaf())));
	}
}
