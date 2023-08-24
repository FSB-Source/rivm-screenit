package nl.rivm.screenit.dao.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.dao.cervix.CervixHuisartsLocatieDao;
import nl.rivm.screenit.huisartsenportaal.enums.CervixLocatieStatus;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsAanmeldStatus;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsLocatieMutatieSoort;
import nl.rivm.screenit.service.cervix.CervixHuisartsLocatieFilter;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.criteria.BaseCriteria;
import nl.topicuszorg.hibernate.criteria.ListCriteria;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.MatchMode;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.hibernate.sql.JoinType;
import org.jetbrains.annotations.NotNull;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.primitives.Ints;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class CervixHuisartsLocatieDaoImpl extends AbstractAutowiredDao implements CervixHuisartsLocatieDao
{
	@Override
	public List<CervixHuisartsLocatie> getHuisartsLocaties(CervixHuisartsLocatieFilter filter, long first, long count, String sortProperty, boolean asc)
	{
		return createHuisartsLocatiesBaseCriteria(filter).list(getSession(), new ListCriteria(Ints.checkedCast(first), Ints.checkedCast(count), sortProperty, asc));
	}

	@Override
	public List<CervixHuisartsLocatie> getHuisartsLocaties(long first, long count, String orderByProperty, boolean ascending,
		String agbCode, List<CervixHuisartsLocatieMutatieSoort> mutatiesoorten, Date mutatiedatumVanaf, Date mutatiedatumTot, List<Gemeente> gemeentes)
	{
		BaseCriteria<CervixHuisartsLocatie> baseCriteria = getCervixHuisartsLocatieBaseCriteria(agbCode, mutatiesoorten, mutatiedatumVanaf, mutatiedatumTot, gemeentes);
		if (orderByProperty != null)
		{
			baseCriteria.order(ascending, orderByProperty);
		}

		if (first > 0 || count > 0)
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
	public long countHuisartsLocaties(String agbCode, List<CervixHuisartsLocatieMutatieSoort> mutatiesoorten, Date mutatiedatumVanaf, Date mutatiedatumTot,
		List<Gemeente> gemeentes)
	{
		BaseCriteria<CervixHuisartsLocatie> baseCriteria = getCervixHuisartsLocatieBaseCriteria(agbCode, mutatiesoorten, mutatiedatumVanaf, mutatiedatumTot, gemeentes);

		return baseCriteria.count(getSession());
	}

	private BaseCriteria<CervixHuisartsLocatie> getCervixHuisartsLocatieBaseCriteria(String agbcode, List<CervixHuisartsLocatieMutatieSoort> mutatiesoorten,
		Date mutatiedatumVanaf, Date mutatiedatumTot, List<Gemeente> gemeentes)
	{
		BaseCriteria<CervixHuisartsLocatie> crit = getCervixHuisartsLocatieBaseCriteria(agbcode);

		crit.alias("locatieAdres.woonplaats", "woonplaats");
		crit.alias("woonplaats.gemeente", "gemeente");

		if (mutatiedatumVanaf != null)
		{
			crit.add(Restrictions.ge("mutatiedatum", mutatiedatumVanaf));
		}
		if (mutatiedatumTot != null)
		{
			crit.add(Restrictions.le("mutatiedatum", DateUtil.plusDagen(mutatiedatumTot, 1)));
		}
		if (CollectionUtils.isNotEmpty(mutatiesoorten))
		{
			crit.add(Restrictions.in("mutatieSoort", mutatiesoorten));
		}
		if (!gemeentes.isEmpty())
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
		BaseCriteria<CervixHuisartsLocatie> crit = getCervixHuisartsLocatieBaseCriteria(filter.getAgbcode());

		crit.alias("locatieAdres.woonplaats", "woonplaats", JoinType.LEFT_OUTER_JOIN);
		crit.alias("huisarts.organisatieMedewerkers", "organisatieMedewerker");
		crit.alias("organisatieMedewerker.medewerker", "medewerker");
		if (StringUtils.isNotBlank(filter.getAchternaam()))
		{
			crit.add(Restrictions.ilike("medewerker.achternaam", filter.getAchternaam(), MatchMode.ANYWHERE));
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

	@NotNull
	private static BaseCriteria<CervixHuisartsLocatie> getCervixHuisartsLocatieBaseCriteria(String agbcode)
	{
		BaseCriteria<CervixHuisartsLocatie> crit = new BaseCriteria<>(CervixHuisartsLocatie.class, "huisartsLocatie");
		crit.alias("huisartsLocatie.huisarts", "huisarts");
		crit.alias("huisartsLocatie.locatieAdres", "locatieAdres");

		crit.add(Restrictions.eq("huisarts.aanmeldStatus", CervixHuisartsAanmeldStatus.GEREGISTREERD));
		crit.add(CervixRestrictions.createLocatieCompleetRestriction("huisartsLocatie"));

		if (StringUtils.isNotBlank(agbcode))
		{
			crit.add(Restrictions.like("huisarts.agbcode", agbcode, MatchMode.ANYWHERE));
		}
		return crit;
	}

	@Override
	public CervixHuisarts getActieveHuisartsMetEenActieveLocatie(String agb)
	{
		BaseCriteria<CervixHuisarts> baseCriteria = new BaseCriteria<>(CervixHuisarts.class, "ha");
		baseCriteria.add(Restrictions.eq("ha.agbcode", agb.trim()));
		baseCriteria.add(Restrictions.eq("ha.actief", Boolean.TRUE));

		DetachedCriteria subcriteria = DetachedCriteria.forClass(CervixHuisartsLocatie.class, "locatie");
		subcriteria.add(Restrictions.eq("locatie.status", CervixLocatieStatus.ACTIEF));
		subcriteria.add(Restrictions.eqProperty("locatie.huisarts", "ha.id"));
		subcriteria.setProjection(Projections.rowCount());

		baseCriteria.add(Subqueries.eq(1L, subcriteria));

		return baseCriteria.uniqueResult(getSession());
	}
}
