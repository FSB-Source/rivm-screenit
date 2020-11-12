package nl.rivm.screenit.dao.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.dao.cervix.CervixHuisartsBaseDao;
import nl.rivm.screenit.huisartsenportaal.enums.CervixLocatieStatus;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.Woonplaats;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixLabformulierAanvraag;
import nl.rivm.screenit.model.cervix.CervixRegioBrief;
import nl.rivm.screenit.model.cervix.CervixRegioMergedBrieven;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsAanmeldStatus;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierAanvraagStatus;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.criteria.BaseCriteria;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.Criteria;
import org.hibernate.criterion.Disjunction;
import org.hibernate.criterion.MatchMode;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class CervixHuisartsBaseDaoImpl extends AbstractAutowiredDao implements CervixHuisartsBaseDao
{
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public CervixHuisarts getHuisarts(String agb)
	{
		BaseCriteria<CervixHuisarts> baseCriteria = new BaseCriteria<>(CervixHuisarts.class);
		baseCriteria.add(Restrictions.eq("agbcode", agb.trim()));
		return baseCriteria.uniqueResult(getSession());
	}

	@Override
	public CervixHuisarts getActieveHuisarts(String agb)
	{
		BaseCriteria<CervixHuisarts> baseCriteria = new BaseCriteria<>(CervixHuisarts.class);
		baseCriteria.add(Restrictions.eq("agbcode", agb.trim()));
		baseCriteria.add(Restrictions.eq("actief", Boolean.TRUE));
		return baseCriteria.uniqueResult(getSession());
	}

	@Override
	public CervixRegioBrief getLaatsteRegistratieBrief(CervixHuisarts arts)
	{
		BaseCriteria<CervixRegioBrief> baseCriteria = new BaseCriteria<>(CervixRegioBrief.class);
		baseCriteria.add(Restrictions.eq("briefType", BriefType.REGIO_REGISTRATIE_UITSTRIJKEND_HUISARTS));
		baseCriteria.add(Restrictions.eq("huisarts", arts));
		baseCriteria.addOrder(Order.desc("creatieDatum"));
		return baseCriteria.getFirstResult(getSession());
	}

	@Override
	public List<CervixHuisarts> getUistrijkendArtsen(int first, int count, String sortProperty, boolean ascending, String agbCode, DateTime mutatiedatumVanaf,
		DateTime mutatiedatumTot, Gemeente... gemeentes)
	{
		Criteria baseCriteria = getUitstrijkendArtsenBaseCriteria(agbCode, mutatiedatumVanaf, mutatiedatumTot, gemeentes);

		if (sortProperty != null)
		{
			if (ascending)
			{
				baseCriteria.addOrder(Order.asc(sortProperty));
			}
			else
			{
				baseCriteria.addOrder(Order.desc(sortProperty));
			}
		}
		baseCriteria.setFirstResult(Math.max(first, 0));
		if (count < 0)
		{
			baseCriteria.setMaxResults(Integer.MAX_VALUE);
		}
		else
		{
			baseCriteria.setMaxResults(count);
		}

		baseCriteria.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);

		return baseCriteria.list();
	}

	@Override
	public long countUitstrijkendArtsen(String agbCode, DateTime mutatiedatumVanaf, DateTime mutatiedatumTot, Gemeente... gemeentes)
	{
		Criteria baseCriteria = getUitstrijkendArtsenBaseCriteria(agbCode, mutatiedatumVanaf, mutatiedatumTot, gemeentes);
		baseCriteria.setProjection(Projections.countDistinct("id"));

		@SuppressWarnings("unchecked")
		List<Long> countResult = baseCriteria.list();
		return countResult.get(0);
	}

	private Criteria getUitstrijkendArtsenBaseCriteria(String agbCode, DateTime mutatiedatumVanaf, DateTime mutatiedatumTot, Gemeente... gemeentes)
	{
		Criteria criteria = getSession().createCriteria(CervixHuisarts.class);
		criteria.createAlias("postadres", "postadres");
		criteria.createAlias("postadres.woonplaats", "woonplaats");
		criteria.createAlias("woonplaats.gemeente", "gemeente");

		Disjunction or = Restrictions.disjunction();
		for (Gemeente gemeente : gemeentes)
		{
			or.add(Restrictions.eq("gemeente.id", gemeente.getId()));
		}
		criteria.add(or);
		criteria.add(Restrictions.eq("status", CervixLocatieStatus.ACTIEF));
		criteria.add(Restrictions.eq("aanmeldStatus", CervixHuisartsAanmeldStatus.GEREGISTREERD));

		if (StringUtils.isNotBlank(agbCode))
		{
			criteria.add(Restrictions.like("agbcode", agbCode, MatchMode.ANYWHERE));
		}
		if (mutatiedatumVanaf != null)
		{
			criteria.add(Restrictions.ge("mutatiedatum", mutatiedatumVanaf.toDate()));
		}
		if (mutatiedatumTot != null)
		{
			criteria.add(Restrictions.le("mutatiedatum", mutatiedatumTot.plusDays(1).toDate()));
		}
		return criteria;
	}

	@Override
	public Woonplaats getWoonplaats(String woonplaatsnaam)
	{
		return getWoonplaats(woonplaatsnaam, null);
	}

	@Override
	public Woonplaats getWoonplaats(String woonplaatsnaam, String gemeenteNaam)
	{

		Criteria crit = getSession().createCriteria(Woonplaats.class);
		crit.add(Restrictions.or(Restrictions.eq("naam", woonplaatsnaam).ignoreCase(), Restrictions.eq("naam", "\"" + woonplaatsnaam + "\"").ignoreCase()));
		if (gemeenteNaam != null)
		{
			crit.createAlias("gemeente", "gemeente");
			crit.add(Restrictions.or(Restrictions.eq("gemeente.naam", gemeenteNaam).ignoreCase(), Restrictions.eq("gemeente.naam", "\"" + gemeenteNaam + "\"").ignoreCase()));
		}
		return (Woonplaats) crit.uniqueResult();
	}

	private Criteria getCervixLabformulierOrdersBaseCriteria(CervixHuisarts instelling)
	{
		Criteria crit = getSession().createCriteria(CervixLabformulierAanvraag.class);

		crit.createAlias("instellingGebruiker", "instellingGebruiker");
		crit.createAlias("instellingGebruiker.organisatie", "organisatie");

		crit.createAlias("huisartsLocatie", "locatie");
		crit.add(Restrictions.eq("locatie.huisarts", instelling));
		crit.add(Restrictions.ne("status", CervixLabformulierAanvraagStatus.VERWIJDERD));
		return crit;
	}

	@Override
	public Iterator<CervixLabformulierAanvraag> getCervixLabformulierOrdersVanInstelling(CervixHuisarts instelling, long first, long count, SortState<String> sortState)
	{
		Criteria crit = getCervixLabformulierOrdersBaseCriteria(instelling);
		crit.setMaxResults((int) count);
		crit.setFirstResult((int) first);
		if (sortState.isAsc())
		{
			crit.addOrder(Order.asc(sortState.getSortParam()));
		}
		else
		{
			crit.addOrder(Order.desc(sortState.getSortParam()));
		}
		return crit.list().iterator();
	}

	@Override
	public long getAantalCervixLabformulierOrdersVanInstelling(CervixHuisarts instelling)
	{
		Criteria crit = getCervixLabformulierOrdersBaseCriteria(instelling);
		crit.setProjection(Projections.rowCount());
		return ((Number) crit.uniqueResult()).longValue();
	}

	@Override
	public List<CervixHuisartsLocatie> getActieveHuisartsLocatiesVanHuisarts(CervixHuisarts huisarts)
	{
		CervixHuisartsLocatie locatieZoekObject = new CervixHuisartsLocatie();
		locatieZoekObject.setHuisarts(huisarts);
		locatieZoekObject.setStatus(CervixLocatieStatus.ACTIEF);
		Criteria criteria = getCervixHuisartsLocatieBaseCriteria(locatieZoekObject);
		criteria.add(Restrictions.eq("status", CervixLocatieStatus.ACTIEF));
		return criteria.list();
	}

	private Criteria getCervixHuisartsLocatieBaseCriteria(CervixHuisartsLocatie locatie)
	{
		Criteria crit = getSession().createCriteria(CervixHuisartsLocatie.class);

		if (locatie.getStatus() != null)
		{
			if (CervixLocatieStatus.INACTIEF.equals(locatie.getStatus()))
			{
				crit.add(Restrictions.eq("status", CervixLocatieStatus.INACTIEF));
			}
			else
			{
				crit.add(Restrictions.ne("status", CervixLocatieStatus.INACTIEF));
			}
		}

		crit.createAlias("locatieAdres", "locatieAdres");
		crit.createAlias("locatieAdres.woonplaats", "woonplaats");

		crit.add(Restrictions.eq("huisarts", locatie.getHuisarts()));
		return crit;
	}

	@Override
	public Iterator<CervixHuisartsLocatie> getCervixHuisartsLocatieVanHuisarts(CervixHuisartsLocatie locatie, long first, long count, SortState<String> sortState)
	{
		Criteria crit = getCervixHuisartsLocatieBaseCriteria(locatie);
		crit.setMaxResults((int) count);
		crit.setFirstResult((int) first);
		if (sortState.isAsc())
		{
			crit.addOrder(Order.asc(sortState.getSortParam()));
		}
		else
		{
			crit.addOrder(Order.desc(sortState.getSortParam()));
		}

		return crit.list().iterator();
	}

	@Override
	public long getAantalCervixHuisartsLocatieVanHuisarts(CervixHuisartsLocatie locatie)
	{
		Criteria crit = getCervixHuisartsLocatieBaseCriteria(locatie);
		crit.setProjection(Projections.rowCount());
		return ((Number) crit.uniqueResult()).longValue();
	}

	@Override
	public CervixHuisartsLocatie getCervixHuisartsLocatieWithName(CervixHuisartsLocatie locatie)
	{

		Criteria crit = getSession().createCriteria(CervixHuisartsLocatie.class);
		crit.add(Restrictions.eq("naam", locatie.getNaam()).ignoreCase());
		crit.add(Restrictions.ne("id", locatie.getId()));
		crit.add(Restrictions.eq("huisarts", locatie.getHuisarts()));
		return (CervixHuisartsLocatie) crit.uniqueResult();
	}

	@Override
	public List<CervixLabformulierAanvraag> getCervixLabformulierAanvraagFromMergedBrieven(CervixRegioMergedBrieven cervixRegioMergedBrieven)
	{
		Criteria crit = getSession().createCriteria(CervixLabformulierAanvraag.class);
		crit.createAlias("brief", "brief", JoinType.LEFT_OUTER_JOIN);
		crit.createAlias("voorbladBrief", "voorbladBrief", JoinType.LEFT_OUTER_JOIN);

		crit.add( 
			Restrictions.or(
				Restrictions.eq("brief.mergedBrieven", cervixRegioMergedBrieven), 
				Restrictions.eq("voorbladBrief.mergedBrieven", cervixRegioMergedBrieven) 
			) 
		);

		return crit.list();
	}

	@Override
	public List<CervixLabformulierAanvraag> getNogNietVerstuurdeCervixLabformulierAanvraagVanLocatie(CervixHuisartsLocatie locatie)
	{
		Criteria crit = getSession().createCriteria(CervixLabformulierAanvraag.class);
		crit.add(Restrictions.eq("huisartsLocatie", locatie));
		crit.add(Restrictions.eq("status", CervixLabformulierAanvraagStatus.AANGEVRAAGD));
		return crit.list();
	}
}
