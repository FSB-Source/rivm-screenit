package nl.rivm.screenit.huisartsenportaal.repository.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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

import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import nl.rivm.screenit.huisartsenportaal.dto.TableResultOptionsDto;
import nl.rivm.screenit.huisartsenportaal.dto.VerrichtingFilterDto;
import nl.rivm.screenit.huisartsenportaal.dto.VerrichtingZoekObjectDto;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.Locatie;
import nl.rivm.screenit.huisartsenportaal.model.Locatie_;
import nl.rivm.screenit.huisartsenportaal.model.Verrichting;
import nl.rivm.screenit.huisartsenportaal.model.Verrichting_;
import nl.rivm.screenit.huisartsenportaal.repository.VerrichtingCriteriaRepository;
import nl.rivm.screenit.huisartsenportaal.util.DateUtil;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

@Transactional
@Repository
public class VerrichtingRepositoryImpl extends BaseCustomRepositoryImpl<Verrichting> implements VerrichtingCriteriaRepository
{

	private CriteriaQuery<?> whereVerrichtingen(CriteriaQuery<?> query, Root<Verrichting> verrichtingRoot, Huisarts huisarts,
		VerrichtingZoekObjectDto object)
	{
		CriteriaBuilder cb = getCriteriaBuilder();
		List<Predicate> condities = new ArrayList<>();

		condities.add(cb.equal(verrichtingRoot.get(Verrichting_.huisarts), huisarts));

		VerrichtingFilterDto filterDto = object.getVerrichtingenZoekObject();
		if (filterDto != null)
		{
			if (filterDto.getLocatie() != null)
			{
				Join<Verrichting, Locatie> locatiesJoin = verrichtingRoot.join(Verrichting_.huisartsLocatie);
				condities.add(cb.equal(locatiesJoin.get(Locatie_.huisartsportaalId), filterDto.getLocatie().getHuisartsportaalId()));
			}
			if (StringUtils.isNotEmpty(filterDto.getClientNaam()))
			{
				condities.add(cb.like(cb.lower(verrichtingRoot.get(Verrichting_.clientNaam)), "%" + StringUtils.lowerCase(filterDto.getClientNaam()) + "%"));
			}
			if (filterDto.getVerrichtingsDatumVanaf() != null)
			{
				condities.add(cb.greaterThanOrEqualTo(verrichtingRoot.get(Verrichting_.verrichtingsDatum), filterDto.getVerrichtingsDatumVanaf()));
			}
			if (filterDto.getVerrichtingsDatumTotenmet() != null)
			{
				condities.add(cb.lessThanOrEqualTo(verrichtingRoot.get(Verrichting_.verrichtingsDatum),
					DateUtil.plusTijdseenheid(filterDto.getVerrichtingsDatumTotenmet(), 1, ChronoUnit.DAYS)));
			}
			if (filterDto.getDatumUitstrijkje() != null)
			{
				condities.add(cb.equal(verrichtingRoot.get(Verrichting_.datumUitstrijkje), filterDto.getDatumUitstrijkje()));
			}
		}
		if (CollectionUtils.isNotEmpty(condities))
		{
			query.where(condities.toArray(new Predicate[condities.size()]));
		}
		return query;
	}

	@Override
	public List<Verrichting> getVerrichtingen(Huisarts huisarts, VerrichtingZoekObjectDto verrichtingZoekObjectDto, TableResultOptionsDto resultOptions)
	{
		CriteriaBuilder cb = getCriteriaBuilder();
		CriteriaQuery<Verrichting> query = cb.createQuery(Verrichting.class);
		Root<Verrichting> verrichtingRoot = query.from(Verrichting.class);
		Join<Verrichting, Locatie> locatieJoin = verrichtingRoot.join(Verrichting_.huisartsLocatie, JoinType.LEFT);

		query.select(verrichtingRoot);

		whereVerrichtingen(query, verrichtingRoot, huisarts, verrichtingZoekObjectDto);

		if (resultOptions.getSortOptions() != null && !resultOptions.getSortOptions().isEmpty())
		{
			Map.Entry<String, String> entry = resultOptions.getSortOptions().entrySet().iterator().next();
			From orderByObject = verrichtingRoot;
			String filter = StringUtils.remove(entry.getKey(), '.'); 
			if (StringUtils.startsWith(filter, "huisartsLocatie"))
			{
				filter = filter.replace("huisartsLocatie", "");
				orderByObject = locatieJoin;
			}
			if (entry.getValue().equalsIgnoreCase("desc"))
			{
				query.orderBy(cb.desc(orderByObject.get(filter)));
			}
			else
			{
				query.orderBy(cb.asc(orderByObject.get(filter)));
			}
		}

		if (resultOptions.getCount() > -1 && resultOptions.getFirst() > -1)
		{
			return getResultList(query, resultOptions.getFirst(), resultOptions.getCount());
		}

		return getResultList(query);
	}

	@Override
	public long countVerrichtingen(Huisarts huisarts, VerrichtingZoekObjectDto verrichtingZoekObjectDto)
	{
		CriteriaBuilder cb = getCriteriaBuilder();
		CriteriaQuery<Long> query = cb.createQuery(Long.class);

		Root<Verrichting> verrichtingRoot = query.from(Verrichting.class);
		query.select(cb.count(verrichtingRoot));
		whereVerrichtingen(query, verrichtingRoot, huisarts, verrichtingZoekObjectDto);

		return getEntityManager().createQuery(query).getSingleResult();
	}
}
