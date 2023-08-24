package nl.rivm.screenit.main.dao.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.List;

import nl.rivm.screenit.dao.cervix.impl.CervixRestrictions;
import nl.rivm.screenit.huisartsenportaal.enums.CervixLocatieStatus;
import nl.rivm.screenit.main.dao.cervix.CervixHuisartsDao;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.Woonplaats;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixLabformulierAanvraag;
import nl.rivm.screenit.model.cervix.CervixRegioBrief;
import nl.rivm.screenit.model.cervix.CervixRegioMergedBrieven;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierAanvraagStatus;
import nl.rivm.screenit.model.enums.BriefType;
import nl.topicuszorg.hibernate.criteria.BaseCriteria;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.primitives.Ints;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class CervixHuisartsDaoImpl extends AbstractAutowiredDao implements CervixHuisartsDao
{

	@Override
	public CervixHuisarts getHuisarts(String agb)
	{
		BaseCriteria<CervixHuisarts> baseCriteria = new BaseCriteria<>(CervixHuisarts.class);
		baseCriteria.add(Restrictions.eq("agbcode", agb.trim()));
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
	public List<CervixLabformulierAanvraag> getCervixLabformulierOrdersVanInstelling(CervixHuisarts instelling, long first, long count, SortState<String> sortState)
	{
		Criteria crit = getCervixLabformulierOrdersBaseCriteria(instelling);
		return getPagedAndSortedResultList(crit, first, count, sortState);
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
		Criteria criteria = getCervixHuisartsLocatieBaseCriteria(locatieZoekObject);
		criteria.add(Restrictions.eq("status", CervixLocatieStatus.ACTIEF));
		criteria.add(CervixRestrictions.createLocatieCompleetRestriction(""));
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
	public List<CervixHuisartsLocatie> getCervixHuisartsLocatieVanHuisarts(CervixHuisartsLocatie locatie, long first, long count, SortState<String> sortState)
	{
		Criteria crit = getCervixHuisartsLocatieBaseCriteria(locatie);
		return getPagedAndSortedResultList(crit, first, count, sortState);
	}

	private List getPagedAndSortedResultList(Criteria crit, long first, long count, SortState<String> sortState)
	{
		crit.setMaxResults(Ints.checkedCast(count));
		crit.setFirstResult(Ints.checkedCast(first));
		if (sortState.isAsc())
		{
			crit.addOrder(Order.asc(sortState.getSortParam()));
		}
		else
		{
			crit.addOrder(Order.desc(sortState.getSortParam()));
		}

		return crit.list();
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
