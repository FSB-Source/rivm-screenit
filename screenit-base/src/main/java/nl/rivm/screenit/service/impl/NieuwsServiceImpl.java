package nl.rivm.screenit.service.impl;

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

import java.util.List;
import java.util.stream.Collectors;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.nieuws.NieuwsItem;
import nl.rivm.screenit.repository.algemeen.NieuwsRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.NieuwsService;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.repository.algemeen.NieuwsRepository.baseSpecification;
import static nl.rivm.screenit.repository.algemeen.NieuwsRepository.isOngelezen;
import static nl.rivm.screenit.repository.algemeen.NieuwsRepository.publicerenTot;
import static nl.rivm.screenit.repository.algemeen.NieuwsRepository.publicerenVanaf;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
@AllArgsConstructor
public class NieuwsServiceImpl implements NieuwsService
{
	private final NieuwsRepository nieuwsRepository;

	private final ICurrentDateSupplier dateSupplier;

	@Override
	public List<NieuwsItem> getNieuwsItems(boolean inclusiefVerlopenNieuwsItems)
	{
		Specification<NieuwsItem> specification = baseSpecification();
		if (!inclusiefVerlopenNieuwsItems)
		{
			specification = specification.and(publicerenTot(dateSupplier.getDate()));
		}
		return nieuwsRepository.findAll(specification, Sort.by(Sort.Order.desc("publicerenVanaf")));
	}

	@Override
	public List<Long> getNieuwsItemIdsGebruiker(Gebruiker gebruiker)
	{
		return nieuwsRepository.findAll(baseSpecification()
			.and(isOngelezen(gebruiker))
			.and(publicerenVanaf(dateSupplier.getDate())),
			Sort.by(Sort.Order.desc("publicerenVanaf")))
			.stream().map(AbstractHibernateObject::getId).collect(Collectors.toList());
	}

}
