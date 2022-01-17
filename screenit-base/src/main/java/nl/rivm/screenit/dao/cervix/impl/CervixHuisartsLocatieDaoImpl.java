package nl.rivm.screenit.dao.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.List;

import nl.rivm.screenit.dao.cervix.CervixHuisartsLocatieDao;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsAanmeldStatus;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsLocatieMutatieSoort;
import nl.rivm.screenit.service.cervix.CervixHuisartsLocatieFilter;
import nl.topicuszorg.hibernate.criteria.BaseCriteria;
import nl.topicuszorg.hibernate.criteria.ListCriteria;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.hibernate.criterion.MatchMode;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.joda.time.DateTime;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.primitives.Ints;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class CervixHuisartsLocatieDaoImpl extends AbstractAutowiredDao implements CervixHuisartsLocatieDao
{
	@Override
	public List<CervixHuisartsLocatie> getHuisartsLocaties(CervixHuisartsLocatieFilter filter, long first, long count, String sortProperty, boolean asc)
	{
		return createHuisartsLocatiesBaseCriteria(filter).list(getSession(), new ListCriteria(Ints.checkedCast(first), Ints.checkedCast(count), sortProperty, asc));
	}

	@Override
	public List<CervixHuisartsLocatie> getHuisartsLocaties(long first, long count, String orderByProperty, boolean ascending,
		String agbCode, List<CervixHuisartsLocatieMutatieSoort> mutatiesoorten, DateTime mutatiedatumVanaf, DateTime mutatiedatumTot, Gemeente... gemeentes)
	{
		BaseCriteria<CervixHuisartsLocatie> baseCriteria = getCervixHuisartsLocatieBaseCriteria(agbCode, mutatiesoorten, mutatiedatumVanaf, mutatiedatumTot, gemeentes);
		if (orderByProperty != null)
		{
			baseCriteria.order(ascending, orderByProperty);
		}

		if (first > 0 && count > 0 || first > 0 || count > 0)
		{
			return baseCriteria.list(getSession(), new ListCriteria(Ints.checkedCast(first), Ints.checkedCast(count), orderByProperty, ascending));
		}
		else
		{
			return baseCriteria.list(getSession());
		}
	}

	@Override
	public long countHuisartsLocaties(CervixHuisartsLocatieFilter filter)
	{
		return createHuisartsLocatiesBaseCriteria(filter).count(getSession());
	}

	@Override
	public long countHuisartsLocaties(String agbCode, List<CervixHuisartsLocatieMutatieSoort> mutatiesoorten, DateTime mutatiedatumVanaf, DateTime mutatiedatumTot,
		Gemeente... gemeentes)
	{
		BaseCriteria<CervixHuisartsLocatie> baseCriteria = getCervixHuisartsLocatieBaseCriteria(agbCode, mutatiesoorten, mutatiedatumVanaf, mutatiedatumTot, gemeentes);

		return baseCriteria.count(getSession());
	}

	private BaseCriteria<CervixHuisartsLocatie> getCervixHuisartsLocatieBaseCriteria(String agbCode, List<CervixHuisartsLocatieMutatieSoort> mutatiesoorten,
		DateTime mutatiedatumVanaf,
		DateTime mutatiedatumTot,
		Gemeente... gemeentes)
	{
		BaseCriteria<CervixHuisartsLocatie> crit = new BaseCriteria<>(CervixHuisartsLocatie.class, "huisartsLocatie");
		crit.alias("huisartsLocatie.huisarts", "huisarts");
		crit.alias("huisartsLocatie.locatieAdres", "locatieAdres");
		crit.alias("locatieAdres.woonplaats", "woonplaats");
		crit.alias("woonplaats.gemeente", "gemeente");

		crit.add(Restrictions.eq("huisarts.aanmeldStatus", CervixHuisartsAanmeldStatus.GEREGISTREERD));

		if (StringUtils.isNotBlank(agbCode))
		{
			crit.add(Restrictions.like("huisarts.agbcode", agbCode, MatchMode.ANYWHERE));
		}
		if (mutatiedatumVanaf != null)
		{
			crit.add(Restrictions.ge("mutatiedatum", mutatiedatumVanaf.toDate()));
		}
		if (mutatiedatumTot != null)
		{
			crit.add(Restrictions.le("mutatiedatum", mutatiedatumTot.plusDays(1).toDate()));
		}
		if (CollectionUtils.isNotEmpty(mutatiesoorten))
		{
			crit.add(Restrictions.in("mutatieSoort", mutatiesoorten));
		}
		if (gemeentes.length > 0)
		{
			List<Long> ids = new ArrayList<>();
			for (Gemeente gemeente : gemeentes)
			{
				ids.add(gemeente.getId());
			}

			crit.in("gemeente.id", ids);
		}
		else
		{
			crit.alias("gemeente.bmhkLaboratorium", "lab");
			crit.add(Restrictions.eq("lab.naam", "")); 
		}
		return crit;
	}

	private BaseCriteria<CervixHuisartsLocatie> createHuisartsLocatiesBaseCriteria(CervixHuisartsLocatieFilter filter)
	{
		BaseCriteria<CervixHuisartsLocatie> crit = new BaseCriteria<>(CervixHuisartsLocatie.class, "huisartsLocatie");
		crit.alias("huisartsLocatie.huisarts", "huisarts");
		crit.alias("huisarts.organisatieMedewerkers", "organisatieMedewerker");
		crit.alias("organisatieMedewerker.medewerker", "medewerker");
		crit.alias("huisartsLocatie.locatieAdres", "locatieAdres");
		crit.alias("locatieAdres.woonplaats", "woonplaats", JoinType.LEFT_OUTER_JOIN);

		crit.add(Restrictions.eq("huisarts.aanmeldStatus", CervixHuisartsAanmeldStatus.GEREGISTREERD));

		if (StringUtils.isNotBlank(filter.getAchternaam()))
		{
			crit.add(Restrictions.ilike("medewerker.achternaam", filter.getAchternaam(), MatchMode.ANYWHERE));
		}

		if (StringUtils.isNotBlank(filter.getAgbcode()))
		{
			crit.add(Restrictions.ilike("huisarts.agbcode", filter.getAgbcode(), MatchMode.ANYWHERE));
		}

		if (StringUtils.isNotBlank(filter.getLocatieNaam()))
		{
			crit.add(Restrictions.ilike("huisartsLocatie.naam", filter.getLocatieNaam(), MatchMode.ANYWHERE));
		}

		if (StringUtils.isNotBlank(filter.getPostcode()))
		{
			String postcode = StringUtils.deleteWhitespace(filter.getPostcode());
			crit.add(Restrictions.ilike("locatieAdres.postcode", postcode, MatchMode.ANYWHERE));
		}

		if (StringUtils.isNotBlank(filter.getPlaats()))
		{
			crit.add(Restrictions.ilike("woonplaats.naam", filter.getPlaats(), MatchMode.ANYWHERE));
		}

		if (StringUtils.isNotBlank(filter.getStraat()))
		{
			crit.add(Restrictions.ilike("locatieAdres.straat", filter.getStraat(), MatchMode.ANYWHERE));
		}

		return crit;
	}
}
