package nl.rivm.screenit.huisartsenportaal.repository.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2016 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.ArrayList;
import java.util.EnumSet;
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
import nl.rivm.screenit.huisartsenportaal.dto.VerrichtingZoekObjectDto;
import nl.rivm.screenit.huisartsenportaal.dto.locatie.LocatieSearchDto;
import nl.rivm.screenit.huisartsenportaal.enums.CervixLocatieStatus;
import nl.rivm.screenit.huisartsenportaal.model.Adres;
import nl.rivm.screenit.huisartsenportaal.model.Adres_;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.Locatie;
import nl.rivm.screenit.huisartsenportaal.model.Locatie_;
import nl.rivm.screenit.huisartsenportaal.model.Verrichting;
import nl.rivm.screenit.huisartsenportaal.model.Woonplaats;
import nl.rivm.screenit.huisartsenportaal.repository.LocatieCriteriaRepository;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

@Transactional
@Repository
public class LocatieCriteriaRepositoryImpl extends BaseCustomRepositoryImpl implements LocatieCriteriaRepository
{

	@Override
	public List<Locatie> getLocaties(Huisarts huisarts, LocatieSearchDto locatieSearchDto)
	{

		CriteriaQuery<Locatie> query = getCriteriaBuilder().createQuery(Locatie.class);

		Root<Locatie> locatieRoot = query.from(Locatie.class);
		Join<Locatie, Adres> adresJoin = locatieRoot.join(Locatie_.locatieAdres, JoinType.LEFT);
		Join<Adres, Woonplaats> woonplaatsJoin = adresJoin.join(Adres_.woonplaats, JoinType.LEFT);

		query.select(locatieRoot);

		whereLocaties(query, locatieRoot, huisarts, locatieSearchDto);

		TableResultOptionsDto resultOptions = locatieSearchDto.getResultOptions();
		if (resultOptions.getSortOptions() != null && !resultOptions.getSortOptions().isEmpty())
		{
			Map.Entry<String, String> entry = resultOptions.getSortOptions().entrySet().iterator().next();
			From orderByObject = locatieRoot;
			String filter = StringUtils.remove(entry.getKey(), '.'); 
			if (StringUtils.startsWith(filter, "locatieAdres"))
			{
				filter = filter.replace("locatieAdres", "");
				orderByObject = adresJoin;
				if (StringUtils.startsWith(filter, "woonplaats"))
				{
					filter = filter.split("woonplaats")[1];
					orderByObject = woonplaatsJoin;
				}
			}

			if (entry.getValue().equalsIgnoreCase("desc"))
			{
				query.orderBy(getCriteriaBuilder().desc(orderByObject.get(filter)));
			}
			else
			{
				query.orderBy(getCriteriaBuilder().asc(orderByObject.get(filter)));
			}
		}

		if (resultOptions.getCount() > -1 && resultOptions.getFirst() > -1)
		{
			return getResultList(query, resultOptions.getFirst(), resultOptions.getCount());
		}

		return getResultList(query);
	}

	public void whereLocaties(CriteriaQuery<?> query, Root<Locatie> locatieRoot, Huisarts huisarts, LocatieSearchDto locatieSearchDto)
	{
		List<Predicate> condities = new ArrayList<Predicate>();
		condities.add(getCriteriaBuilder().equal(locatieRoot.get(Locatie_.huisarts), huisarts));
		if (locatieSearchDto.getStatus() != null)
		{

			if (CervixLocatieStatus.ACTIEF.equals(CervixLocatieStatus.valueOf(locatieSearchDto.getStatus())))
			{
				condities.add(getCriteriaBuilder().or(getCriteriaBuilder().equal(locatieRoot.get(Locatie_.status), CervixLocatieStatus.valueOf(locatieSearchDto.getStatus())),
						getCriteriaBuilder().equal(locatieRoot.get(Locatie_.status), CervixLocatieStatus.KLANTNUMMER_NIET_GEVERIFIEERD)));
			}
			else
			{
				condities.add(getCriteriaBuilder().equal(locatieRoot.get(Locatie_.status), CervixLocatieStatus.valueOf(locatieSearchDto.getStatus())));
			}
		}
		if (CollectionUtils.isNotEmpty(condities))
		{
			query.where(condities.toArray(new Predicate[condities.size()]));
		}
	}

	@Override
	public long countLocaties(Huisarts huisarts, LocatieSearchDto locatieSearchDto)
	{
		CriteriaBuilder cb = getCriteriaBuilder();
		CriteriaQuery<Long> query = cb.createQuery(Long.class);

		Root<Locatie> locatieRoot = query.from(Locatie.class);
		query.select(cb.count(locatieRoot));
		whereLocaties(query, locatieRoot, huisarts, locatieSearchDto);

		return getEntityManager().createQuery(query).getSingleResult();
	}

	public List<Locatie> findByHuisartsAndStatussen(Huisarts huisarts, EnumSet<CervixLocatieStatus> statussen)
	{
		CriteriaQuery<Locatie> query = getCriteriaBuilder().createQuery(Locatie.class);

		Root<Locatie> locatieRoot = query.from(Locatie.class);
		Join<Locatie, Huisarts> huisartsJoin = locatieRoot.join(Locatie_.huisarts, JoinType.INNER);

		query.select(locatieRoot);

		List<Predicate> condities = new ArrayList<Predicate>();

		condities.add(getCriteriaBuilder().equal(locatieRoot.get(Locatie_.huisarts), huisarts));

		Predicate or = getCriteriaBuilder().disjunction();
		for (CervixLocatieStatus status : statussen)
		{
			or.getExpressions().add(getCriteriaBuilder().equal(locatieRoot.get(Locatie_.status), status));
		}

		condities.add(or);
		if (CollectionUtils.isNotEmpty(condities))
		{
			query.where(condities.toArray(new Predicate[condities.size()]));
		}

		return getResultList(query);

	}
}
