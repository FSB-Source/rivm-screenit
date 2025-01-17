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

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Root;

import nl.rivm.screenit.main.dto.mamma.MammaTehuisDto;
import nl.rivm.screenit.main.service.RepositoryDataProviderService;
import nl.rivm.screenit.main.service.mamma.IMammaTehuisDto;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.tehuis.MammaTehuisFilter;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaats_;
import nl.rivm.screenit.model.mamma.MammaTehuis;
import nl.rivm.screenit.model.mamma.MammaTehuis_;
import nl.rivm.screenit.repository.mamma.MammaTehuisRepository;
import nl.rivm.screenit.service.mamma.MammaBaseTehuisService;

import org.apache.wicket.extensions.markup.html.repeater.util.SortParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.support.PropertyComparator;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.mamma.MammaTehuisSpecification.filterActief;
import static nl.rivm.screenit.specification.mamma.MammaTehuisSpecification.filterNaam;
import static nl.rivm.screenit.specification.mamma.MammaTehuisSpecification.filterStandplaats;
import static nl.rivm.screenit.specification.mamma.MammaTehuisSpecification.heeftStandplaatsInRegio;

@Service("MammaBaseTehuisDataProviderService")
public class MammaBaseTehuisDataProviderServiceImpl extends RepositoryDataProviderService<MammaTehuis, MammaTehuisRepository, MammaTehuisFilter>
{
	@Autowired
	private MammaBaseTehuisService tehuisService;

	@Override
	protected Specification<MammaTehuis> getSpecification(MammaTehuisFilter filter, Sort sortParam)
	{
		return filterNaam(filter.getTehuis().getNaam())
			.and(filterStandplaats(filter.getTehuis().getStandplaats()))
			.and(heeftStandplaatsInRegio(filter.getRegio()))
			.and(filterActief(filter.getActief()));
	}

	@Override
	protected Order addJoinsForSortingOrCreateDedicatedOrders(Sort.Order order, Root<MammaTehuis> r, CriteriaBuilder cb)
	{
		if (order.getProperty().startsWith(MammaStandplaats_.REGIO))
		{
			var standplaatsJoin = join(r, MammaTehuis_.standplaats);
			join(standplaatsJoin, MammaStandplaats_.regio);
		}
		return super.addJoinsForSortingOrCreateDedicatedOrders(order, r, cb);
	}

	public List<IMammaTehuisDto> zoekTehuizen(MammaTehuisFilter filter, long first, long count, SortParam<String> sort)
	{
		var sortProperty = sort.getProperty();
		List<MammaTehuis> tehuizen;
		SortParam<String> newSort;

		if (sortProperty.startsWith("tehuis"))
		{
			newSort = new SortParam<>(sortProperty.replace("tehuis.", ""), sort.isAscending());
			tehuizen = findPage(first, count, filter, newSort);
		}
		else
		{
			tehuizen = findAll(filter);
		}

		var result = tehuizen.stream().map(tehuis ->
		{
			var tehuisDto = new MammaTehuisDto();
			tehuisDto.setTehuis(tehuis);
			var huidigeStandplaatsRonde = tehuisService.getHuidigeStandplaatsRondeVoorStandplaats(tehuis.getStandplaats());
			var eersteStandplaatsPeriode = huidigeStandplaatsRonde != null ? huidigeStandplaatsRonde.getStandplaatsPerioden().stream()
				.min(Comparator.comparing(MammaStandplaatsPeriode::getVanaf)).orElse(null) : null;
			tehuisDto.setStandplaatsPeriode(eersteStandplaatsPeriode);
			return tehuisDto;
		});

		if (!sort.getProperty().startsWith("tehuis"))
		{
			result = result.sorted(new PropertyComparator<>(sort.getProperty(), false, sort.isAscending())).skip(first).limit(count);
		}

		return result.collect(Collectors.toList());
	}
}
