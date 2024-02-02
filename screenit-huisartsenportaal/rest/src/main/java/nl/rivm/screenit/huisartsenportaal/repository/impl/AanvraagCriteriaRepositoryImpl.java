package nl.rivm.screenit.huisartsenportaal.repository.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import nl.rivm.screenit.huisartsenportaal.dto.TableResultOptionsDto;
import nl.rivm.screenit.huisartsenportaal.enums.CervixLocatieStatus;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.LabformulierAanvraag;
import nl.rivm.screenit.huisartsenportaal.model.LabformulierAanvraag_;
import nl.rivm.screenit.huisartsenportaal.model.Locatie;
import nl.rivm.screenit.huisartsenportaal.model.Locatie_;
import nl.rivm.screenit.huisartsenportaal.model.enums.AanvraagStatus;
import nl.rivm.screenit.huisartsenportaal.repository.AanvraagCriteriaRepository;
import nl.rivm.screenit.huisartsenportaal.util.DateUtil;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

@Transactional
@Repository
public class AanvraagCriteriaRepositoryImpl extends BaseCustomRepositoryImpl<LabformulierAanvraag> implements AanvraagCriteriaRepository
{

	@Override
	public List<LabformulierAanvraag> findByHuisarts(Huisarts huisarts, TableResultOptionsDto tableResultOptionsDto)
	{
		CriteriaBuilder cb = getCriteriaBuilder();
		CriteriaQuery<LabformulierAanvraag> query = cb.createQuery(LabformulierAanvraag.class);

		Root<LabformulierAanvraag> labformulierAanvraagRoot = createFromAndWhere(huisarts, cb, query);

		query.select(labformulierAanvraagRoot);

		if (tableResultOptionsDto.getSortOptions() != null && !tableResultOptionsDto.getSortOptions().isEmpty())
		{
			Map.Entry<String, String> entry = tableResultOptionsDto.getSortOptions().entrySet().iterator().next();
			From orderByObject = labformulierAanvraagRoot;
			String filter = StringUtils.remove(entry.getKey(), '.'); 
			if (StringUtils.startsWith(filter, "locatie"))
			{
				filter = filter.replace("locatie", "");
				orderByObject = labformulierAanvraagRoot.getJoins().iterator().next();
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

		if (tableResultOptionsDto.getCount() > -1 && tableResultOptionsDto.getFirst() > -1)
		{
			return getResultList(query, tableResultOptionsDto.getFirst(), tableResultOptionsDto.getCount());
		}
		return getResultList(query);
	}

	@Override
	public long countAanvragen(Huisarts huisarts)
	{
		CriteriaBuilder cb = getCriteriaBuilder();
		CriteriaQuery<Long> query = cb.createQuery(Long.class);

		Root<LabformulierAanvraag> labformulierAanvraagRoot = createFromAndWhere(huisarts, cb, query);

		query.select(cb.count(labformulierAanvraagRoot));
		return getEntityManager().createQuery(query).getSingleResult();
	}

	private Root<LabformulierAanvraag> createFromAndWhere(Huisarts huisarts, CriteriaBuilder cb, CriteriaQuery query)
	{

		Root<LabformulierAanvraag> labformulierAanvraagRoot = query.from(LabformulierAanvraag.class);

		Join<LabformulierAanvraag, Locatie> locatieJoin = labformulierAanvraagRoot.join(LabformulierAanvraag_.locatie);

		List<Predicate> condities = new ArrayList<>();
		condities.add(cb.equal(labformulierAanvraagRoot.get(LabformulierAanvraag_.huisarts), huisarts));
		condities.add(cb.notEqual(labformulierAanvraagRoot.get(LabformulierAanvraag_.status), AanvraagStatus.VERWIJDERD));
		condities.add(cb.notEqual(locatieJoin.get(Locatie_.status), CervixLocatieStatus.INACTIEF));

		if (CollectionUtils.isNotEmpty(condities))
		{
			query.where(condities.toArray(new Predicate[condities.size()]));
		}
		return labformulierAanvraagRoot;
	}

	@Override
	public List<LabformulierAanvraag> findByLocatieAndAanvraagDatumBetween(Locatie locatie, LocalDate datum1, LocalDate datum2)
	{
		CriteriaBuilder cb = getCriteriaBuilder();
		CriteriaQuery<LabformulierAanvraag> query = cb.createQuery(LabformulierAanvraag.class);

		Root<LabformulierAanvraag> labformulierAanvraagRoot = query.from(LabformulierAanvraag.class);

		query.select(labformulierAanvraagRoot);

		List<Predicate> condities = new ArrayList<>();
		condities.add(cb.equal(labformulierAanvraagRoot.get(LabformulierAanvraag_.locatie), locatie));
		condities.add(cb.notEqual(labformulierAanvraagRoot.get(LabformulierAanvraag_.status), AanvraagStatus.VERWIJDERD));
		condities.add(cb.between(labformulierAanvraagRoot.get(LabformulierAanvraag_.aanvraagDatum), DateUtil.toUtilDate(datum1), DateUtil.toUtilDate(datum2)));

		if (CollectionUtils.isNotEmpty(condities))
		{
			query.where(condities.toArray(new Predicate[condities.size()]));
		}

		return getResultList(query);
	}

	@Override
	public List<LabformulierAanvraag> findByLocatie(Locatie locatie)
	{
		CriteriaBuilder cb = getCriteriaBuilder();
		CriteriaQuery<LabformulierAanvraag> query = cb.createQuery(LabformulierAanvraag.class);

		Root<LabformulierAanvraag> labformulierAanvraagRoot = query.from(LabformulierAanvraag.class);

		query.select(labformulierAanvraagRoot);

		List<Predicate> condities = new ArrayList<Predicate>();
		condities.add(cb.equal(labformulierAanvraagRoot.get(LabformulierAanvraag_.locatie), locatie));
		condities.add(cb.notEqual(labformulierAanvraagRoot.get(LabformulierAanvraag_.status), AanvraagStatus.VERWIJDERD));

		if (CollectionUtils.isNotEmpty(condities))
		{
			query.where(condities.toArray(new Predicate[condities.size()]));
		}

		return getResultList(query);
	}
}
