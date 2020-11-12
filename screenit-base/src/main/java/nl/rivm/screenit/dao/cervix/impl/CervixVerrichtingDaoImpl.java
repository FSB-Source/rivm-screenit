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

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dao.cervix.CervixVerrichtingDao;
import nl.rivm.screenit.dto.cervix.facturatie.CervixBetalingsZoekObject;
import nl.rivm.screenit.dto.cervix.facturatie.CervixVerrichtingenZoekObject;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.enums.CervixTariefType;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.model.cervix.facturatie.CervixHuisartsTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixLabTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixVerrichting;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;
import nl.topicuszorg.hibernate.restrictions.NvlRestrictions;

import org.apache.commons.lang.StringUtils;
import org.hibernate.Criteria;
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
public class CervixVerrichtingDaoImpl extends AbstractAutowiredDao implements CervixVerrichtingDao
{

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	private Criteria getCriteriaVoorVerrichtingenVoorBetaling(CervixBetalingsZoekObject zoekObject)
	{
		Criteria crit = getSession().createCriteria(CervixVerrichting.class);
		crit.createAlias("laatsteBoekRegel", "boekRegel");
		crit.createAlias("client", "client");
		crit.createAlias("client.persoon", "persoon");
		crit.createAlias("persoon.gbaAdres", "adres");
		crit.createAlias("monster", "monster");
		crit.createAlias("regio", "regio");
		crit.add(Restrictions.isNull("boekRegel.specificatie"));

		if (zoekObject.isVerrichtingenHuisarts() && !zoekObject.isVerrichtingenLaboratorium())
		{
			crit.add(Restrictions.in("type", CervixTariefType.getAlleHuisartsTariefTypes()));
		}
		else if (zoekObject.isVerrichtingenLaboratorium() && !zoekObject.isVerrichtingenHuisarts())
		{
			crit.add(Restrictions.in("type", CervixTariefType.getAlleLabTariefTypes()));
		}
		else
		{
			crit.add(Restrictions.in("type", CervixTariefType.values()));
		}
		if (StringUtils.isNotEmpty(zoekObject.getBsn()))
		{
			crit.add(Restrictions.eq("persoon.bsn", zoekObject.getBsn()));
		}
		if (zoekObject.getGeboortedatum() != null)
		{
			crit.add(Restrictions.eq("persoon.geboortedatum", zoekObject.getGeboortedatum()));
		}
		if (StringUtils.isNotEmpty(zoekObject.getPostcode()))
		{
			crit.add(Restrictions.eq("adres.postode", zoekObject.getPostcode()));
		}
		if (zoekObject.getHuisnummer() != null)
		{
			crit.add(Restrictions.eq("adres.huisnummer", zoekObject.getHuisnummer()));
		}
		if (zoekObject.getVerrichtingsdatumTotEnMet() != null)
		{
			crit.add(Restrictions.lt("verrichtingsDatum", new DateTime(zoekObject.getVerrichtingsdatumTotEnMet()).plusDays(1).toDate()));
		}
		if (zoekObject.getScreeningOrganisatieId() != null)
		{
			crit.add(Restrictions.eq("regio.id", zoekObject.getScreeningOrganisatieId()));
		}
		if (zoekObject.getMonsterId() != null)
		{
			crit.add(Restrictions.eq("monster.monsterId", zoekObject.getMonsterId()));
		}
		crit.add(Restrictions.isNull("boekRegel.specificatie"));
		return crit;

	}

	@Override
	public List<CervixBoekRegel> getVerrichtingenVoorBetaling(CervixBetalingsZoekObject zoekObject, SortState<String> sortState, long first, long count)
	{
		Criteria crit = getCriteriaVoorVerrichtingenVoorBetaling(zoekObject);

		if (sortState != null)
		{
			if (!sortState.isAsc())
			{
				crit.addOrder(Order.desc(sortState.getSortParam()));
			}
			else
			{
				crit.addOrder(Order.asc(sortState.getSortParam()));
			}
		}

		if (count > -1)
		{
			crit.setMaxResults((int) count);
		}
		if (first > -1)
		{
			crit.setFirstResult((int) first);
		}
		crit.setProjection(Projections.property("laatsteBoekRegel"));
		return crit.list();
	}

	@Override
	public List<CervixBoekRegel> getLabVerrichtingen(CervixVerrichtingenZoekObject verrichtingenCriteria, ScreeningOrganisatie screeningOrganisatie,
		BMHKLaboratorium bmhkLaboratorium, SortState<String> sortState,
		long first,
		long count)
	{
		Criteria criteria = getLabVerrichtingenCriteria(verrichtingenCriteria, screeningOrganisatie, bmhkLaboratorium);

		if (sortState.isAsc())
		{
			criteria.addOrder(Order.asc(sortState.getSortParam()));
		}
		else
		{
			criteria.addOrder(Order.desc(sortState.getSortParam()));
		}
		criteria.addOrder(Order.asc("monster.monsterId"));
		criteria.addOrder(Order.asc("id"));
		if (count > -1)
		{
			criteria.setMaxResults((int) count);
		}
		if (first > -1)
		{
			criteria.setFirstResult((int) first);
		}

		return criteria.list();
	}

	private Criteria getLabVerrichtingenCriteria(CervixVerrichtingenZoekObject verrichtingenCriteria, ScreeningOrganisatie screeningOrganisatie, BMHKLaboratorium bmhkLaboratorium)
	{
		Criteria criteria = getBaseVerrichtingenCriteria(verrichtingenCriteria, screeningOrganisatie);
		criteria.createAlias("tarief.bmhkLaboratorium", "bmhkLaboratorium");

		criteria.add(Restrictions.eq("tarief.bmhkLaboratorium", bmhkLaboratorium));
		criteria.add(Restrictions.ne("verrichting.type", CervixTariefType.HUISARTS_UITSTRIJKJE));
		return criteria;
	}

	@Override
	public long countLabVerrichtingen(CervixVerrichtingenZoekObject verrichtingenCriteria, ScreeningOrganisatie screeningOrganisatie, BMHKLaboratorium bmhkLaboratorium)
	{
		Criteria criteria = getLabVerrichtingenCriteria(verrichtingenCriteria, screeningOrganisatie, bmhkLaboratorium);
		criteria.setProjection(Projections.rowCount());
		return ((Number) criteria.uniqueResult()).longValue();
	}

	private Criteria getBaseVerrichtingenCriteria(CervixVerrichtingenZoekObject verrichtingenCriteria, ScreeningOrganisatie screeningOrganisatie)
	{
		Criteria criteria = getSession().createCriteria(CervixBoekRegel.class);
		criteria.createAlias("verrichting", "verrichting");
		criteria.createAlias("verrichting.monster", "monster");
		criteria.createAlias("monster.labformulier", "labformulier", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("verrichting.client", "client");
		criteria.createAlias("client.persoon", "persoon");
		criteria.createAlias("specificatie", "specificatie", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("specificatie.betaalopdrachtRegel", "betaalopdrachtRegel", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("betaalopdrachtRegel.betaalopdracht", "betaalopdracht", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("tarief", "tarief");
		criteria.createAlias("verrichting.regio", "regio");

		if (screeningOrganisatie != null)
		{
			criteria.add(Restrictions.eq("verrichting.regio", screeningOrganisatie));
		}
		if (verrichtingenCriteria.getDebet() != null)
		{
			criteria.add(Restrictions.eq("debet", verrichtingenCriteria.getDebet()));
		}
		if (verrichtingenCriteria.isAlleenVerrichtingen())
		{
			criteria.add(Restrictions.eqProperty("id", "verrichting.laatsteBoekRegel.id"));
		}
		if (verrichtingenCriteria.isAlleenZonderBetalingskenmerk())
		{
			criteria.add(Restrictions.isNull("specificatie"));
		}

		if (verrichtingenCriteria.getVerrichtingsType() != null)
		{
			criteria.add(Restrictions.eq("verrichting.type", verrichtingenCriteria.getVerrichtingsType()));
		}

		if (verrichtingenCriteria.getMonsterId() != null)
		{
			criteria.add(Restrictions.eq("monster.monsterId", verrichtingenCriteria.getMonsterId()));
		}
		if (verrichtingenCriteria.getGeboorteDatum() != null)
		{
			criteria.add(Restrictions.eq("persoon.geboortedatum", verrichtingenCriteria.getGeboorteDatum()));
		}
		if (StringUtils.isNotBlank(verrichtingenCriteria.getBsn()))
		{
			criteria.add(Restrictions.eq("persoon.bsn", verrichtingenCriteria.getBsn()));
		}
		if (verrichtingenCriteria.getVerrichtingsDatumVanaf() != null)
		{
			criteria.add(Restrictions.ge("verrichting.verrichtingsDatum", verrichtingenCriteria.getVerrichtingsDatumVanaf()));
		}
		if (verrichtingenCriteria.getVerrichtingsDatumTotenmet() != null)
		{
			criteria.add(Restrictions.lt("verrichting.verrichtingsDatum", new DateTime(verrichtingenCriteria.getVerrichtingsDatumTotenmet()).plusDays(1).toDate()));
		}
		if (StringUtils.isNotBlank(verrichtingenCriteria.getBetalingskenmerk()))
		{
			criteria.add(Restrictions.like("betaalopdracht.betalingskenmerk", verrichtingenCriteria.getBetalingskenmerk(), MatchMode.ANYWHERE));
		}

		return criteria;
	}

	private Criteria getHuisartsVerrichtingenCriteria(CervixVerrichtingenZoekObject verrichtingenCriteria, ScreeningOrganisatie screeningOrganisatie, CervixHuisarts huisarts,
		CervixHuisartsLocatie huisartsLocatie)
	{
		Criteria criteria = getBaseVerrichtingenCriteria(verrichtingenCriteria, screeningOrganisatie);
		criteria.createAlias("verrichting.huisartsLocatie", "huisartsLocatie");
		if (huisartsLocatie != null)
		{
			criteria.add(Restrictions.eq("verrichting.huisartsLocatie", huisartsLocatie));
		}
		if (verrichtingenCriteria.getDatumUitstrijkje() != null)
		{
			criteria.add(Restrictions.eq("labformulier.datumUitstrijkje", verrichtingenCriteria.getDatumUitstrijkje()));
		}

		criteria.add(Restrictions.eq("huisartsLocatie.huisarts", huisarts));
		criteria.add(Restrictions.eq("verrichting.type", CervixTariefType.HUISARTS_UITSTRIJKJE));
		return criteria;
	}

	@Override
	public List<CervixBoekRegel> getHuisartsVerrichtingen(CervixVerrichtingenZoekObject verrichtingenCriteria, ScreeningOrganisatie screeningOrganisatie, CervixHuisarts huisarts,
		CervixHuisartsLocatie huisartsLocatie, SortState<String> sortState, long first, long count)
	{
		Criteria criteria = getHuisartsVerrichtingenCriteria(verrichtingenCriteria, screeningOrganisatie, huisarts, huisartsLocatie);

		if (sortState != null)
		{
			if (sortState.isAsc())
			{
				criteria.addOrder(Order.asc(sortState.getSortParam()));
			}
			else
			{
				criteria.addOrder(Order.desc(sortState.getSortParam()));
			}
			criteria.addOrder(Order.asc("monster.monsterId"));
			criteria.addOrder(Order.asc("id"));
		}

		if (count > -1)
		{
			criteria.setMaxResults((int) count);
		}
		if (first > -1)
		{
			criteria.setFirstResult((int) first);
		}

		return criteria.list();
	}

	@Override
	public long countHuisartsVerrichtingen(CervixVerrichtingenZoekObject verrichtingenCriteria, ScreeningOrganisatie screeningOrganisatie, CervixHuisarts huisarts,
		CervixHuisartsLocatie huisartsLocatie)
	{
		Criteria criteria = getHuisartsVerrichtingenCriteria(verrichtingenCriteria, screeningOrganisatie, huisarts, huisartsLocatie);
		criteria.setProjection(Projections.rowCount());
		return ((Number) criteria.uniqueResult()).longValue();
	}

	@Override
	public List<CervixHuisartsTarief> getCervixHuisartsTarieven(CervixHuisartsTarief tarief, long first, long count, SortState<String> sortState)
	{
		Criteria crit = getSession().createCriteria(CervixHuisartsTarief.class);
		crit.add(Restrictions.eq("actief", Boolean.TRUE));
		if (!sortState.isAsc())
		{
			crit.addOrder(Order.desc(sortState.getSortParam()));
		}
		else
		{
			crit.addOrder(Order.asc(sortState.getSortParam()));
		}

		if (count > -1)
		{
			crit.setMaxResults((int) count);
		}
		if (first > -1)
		{
			crit.setFirstResult((int) first);
		}
		return crit.list();
	}

	@Override
	public CervixHuisartsTarief getLatestCervixHuisartsTarief()
	{
		Criteria crit = getSession().createCriteria(CervixHuisartsTarief.class);
		crit.add(Restrictions.eq("actief", Boolean.TRUE));
		crit.add(Restrictions.isNull("geldigTotenmetDatum"));
		return (CervixHuisartsTarief) crit.setMaxResults(1).uniqueResult();
	}

	@Override
	public CervixLabTarief getLatestCervixLabTarief(BMHKLaboratorium lab)
	{
		Criteria crit = getSession().createCriteria(CervixLabTarief.class);
		crit.add(Restrictions.eq("actief", Boolean.TRUE));
		crit.add(Restrictions.eq("bmhkLaboratorium", lab));
		crit.add(Restrictions.isNull("geldigTotenmetDatum"));
		return (CervixLabTarief) crit.setMaxResults(1).uniqueResult();
	}

	@Override
	public Long countCervixHuisartsTarieven(CervixHuisartsTarief tarief)
	{
		Criteria crit = getSession().createCriteria(CervixHuisartsTarief.class);
		crit.add(Restrictions.eq("actief", Boolean.TRUE));
		crit.setProjection(Projections.rowCount());
		return ((Number) crit.uniqueResult()).longValue();
	}

	@Override
	public CervixHuisartsTarief getCervixHuisartsTarief(Date datum)
	{
		Criteria crit = getSession().createCriteria(CervixHuisartsTarief.class);
		crit.add(Restrictions.eq("geldigVanafDatum", datum));
		crit.add(Restrictions.eq("actief", Boolean.TRUE));
		return (CervixHuisartsTarief) crit.uniqueResult();
	}

	@Override
	public CervixLabTarief getCervixLaboratoriumTarief(CervixLabTarief tarief)
	{

		Criteria crit = getSession().createCriteria(CervixLabTarief.class);
		crit.add(Restrictions.eq("actief", Boolean.TRUE));
		crit.add(Restrictions.eq("geldigVanafDatum", tarief.getGeldigVanafDatum()));
		crit.add(Restrictions.eq("bmhkLaboratorium", tarief.getBmhkLaboratorium()));
		return (CervixLabTarief) crit.uniqueResult();
	}

	@Override
	public List<CervixHuisartsTarief> getCervixHuisartsTarievenZonderEinddatum()
	{
		Date vandaag = currentDateSupplier.getDate();
		Criteria crit = getSession().createCriteria(CervixHuisartsTarief.class);
		crit.add(Restrictions.eq("actief", Boolean.TRUE));
		crit.add(NvlRestrictions.ge("geldigTotenmetDatum", vandaag, Constants.END_OF_TIME));
		crit.addOrder(Order.asc("geldigVanafDatum"));

		return crit.list();
	}

	@Override
	public List<CervixLabTarief> getCervixLabTarievenZonderEinddatum(BMHKLaboratorium lab)
	{
		Date vandaag = currentDateSupplier.getDate();
		Criteria crit = getSession().createCriteria(CervixLabTarief.class);
		if (lab != null)
		{
			crit.add(Restrictions.eq("bmhkLaboratorium", lab));
		}
		crit.add(Restrictions.eq("actief", Boolean.TRUE));
		crit.add(NvlRestrictions.ge("geldigTotenmetDatum", vandaag, Constants.END_OF_TIME));
		crit.addOrder(Order.asc("geldigVanafDatum"));

		return crit.list();
	}

	private Criteria getBaseCervixLabTarievenCriteria(CervixLabTarief tarief)
	{
		Criteria crit = getSession().createCriteria(CervixLabTarief.class);
		crit.add(Restrictions.eq("actief", Boolean.TRUE));
		crit.add(Restrictions.eq("bmhkLaboratorium", tarief.getBmhkLaboratorium()));
		return crit;
	}

	@Override
	public List<CervixLabTarief> getCervixLabTarieven(CervixLabTarief zoekobject, long first, long count, SortState<String> sortState)
	{
		Criteria crit = getBaseCervixLabTarievenCriteria(zoekobject);
		if (!sortState.isAsc())
		{
			crit.addOrder(Order.desc(sortState.getSortParam()));
		}
		else
		{
			crit.addOrder(Order.asc(sortState.getSortParam()));
		}

		if (count > -1)
		{
			crit.setMaxResults((int) count);
		}
		if (first > -1)
		{
			crit.setFirstResult((int) first);
		}

		return crit.list();
	}

	@Override
	public Long countCervixLabTarieven(CervixLabTarief zoekobject)
	{
		Criteria crit = getBaseCervixLabTarievenCriteria(zoekobject);
		crit.setProjection(Projections.rowCount());
		return ((Number) crit.uniqueResult()).longValue();
	}

	@Override
	public CervixTarief getGeldigForDatumTarief(CervixTariefType tariefType, Date datum, BMHKLaboratorium bmhkLaboratorium)
	{
		Criteria criteria;
		if (tariefType.equals(CervixTariefType.HUISARTS_UITSTRIJKJE))
		{
			criteria = getSession().createCriteria(CervixHuisartsTarief.class);
		}
		else
		{
			criteria = getSession().createCriteria(CervixLabTarief.class);
			criteria.add(Restrictions.eq("bmhkLaboratorium", bmhkLaboratorium));
		}
		criteria.add(Restrictions.le("geldigVanafDatum", datum));
		criteria.add(NvlRestrictions.ge("geldigTotenmetDatum", datum, Constants.END_OF_TIME));
		criteria.add(Restrictions.eq("actief", Boolean.TRUE));
		return (CervixTarief) criteria.uniqueResult();
	}

	@Override
	public List<CervixBetaalopdracht> getCervixBetaalopdrachten(ScreeningOrganisatie organisatie, SortState<String> sortState, long first, long count)
	{
		Criteria crit = getSession().createCriteria(CervixBetaalopdracht.class);
		if (!sortState.isAsc())
		{
			crit.addOrder(Order.desc(sortState.getSortParam()));
		}
		else
		{
			crit.addOrder(Order.asc(sortState.getSortParam()));
		}

		if (count > -1)
		{
			crit.setMaxResults((int) count);
		}
		if (first > -1)
		{
			crit.setFirstResult((int) first);
		}
		crit.add(Restrictions.eq("screeningOrganisatie", organisatie));
		crit.add(Restrictions.ne("status", BestandStatus.VERWIJDERD));
		return crit.list();
	}

	@Override
	public Long countCervixBetaalOpdrachten(ScreeningOrganisatie organisatie)
	{
		Criteria crit = getSession().createCriteria(CervixBetaalopdracht.class);
		crit.add(Restrictions.eq("screeningOrganisatie", organisatie));
		crit.add(Restrictions.ne("status", BestandStatus.VERWIJDERD));
		crit.setProjection(Projections.rowCount());
		return ((Number) crit.uniqueResult()).longValue();
	}

	@Override
	public List<CervixBetaalopdracht> getVandaagGemaakteBetaalOpdrachten()
	{
		DateTime nu = currentDateSupplier.getDateTimeMidnight();
		Criteria crit = getSession().createCriteria(CervixBetaalopdracht.class);
		crit.add( 
			Restrictions.and( 
				Restrictions.gt("statusDatum", nu.toDate()),
				Restrictions.lt("statusDatum", nu.plusDays(1).toDate())));

		return crit.list();
	}

	@Override
	public BigDecimal getLaboratoriumTotaalBedrag(CervixVerrichtingenZoekObject verrichtingenCriteria, ScreeningOrganisatie screeningOrganisatie, BMHKLaboratorium bmhkLaboratorium,
		CervixTariefType tariefType)
	{
		Criteria criteria = getLabVerrichtingenCriteria(verrichtingenCriteria, screeningOrganisatie, bmhkLaboratorium);
		criteria.add(Restrictions.eq("verrichting.type", tariefType));
		criteria.setProjection(Projections.sum("tarief." + tariefType.getBedragProperty()));
		return (BigDecimal) criteria.uniqueResult();
	}

	@Override
	public BigDecimal getHuisartsTotaalBedrag(CervixVerrichtingenZoekObject verrichtingenCriteria, ScreeningOrganisatie screeningOrganisatie, CervixHuisarts huisarts,
		CervixHuisartsLocatie huisartsLocatie)
	{
		Criteria criteria = getHuisartsVerrichtingenCriteria(verrichtingenCriteria, screeningOrganisatie, huisarts, huisartsLocatie);
		criteria.setProjection(Projections.sum("tarief.tarief"));
		return (BigDecimal) criteria.uniqueResult();
	}

	@Override
	public List<CervixVerrichting> getVerrichtingenVoorTarief(Long tariefId, CervixTarief nieuweTarief, CervixTariefType tariefType)
	{
		Criteria criteria = getSession().createCriteria(CervixVerrichting.class);
		criteria.createAlias("laatsteBoekRegel", "boekRegel");
		criteria.add(Restrictions.eq("boekRegel.tarief.id", tariefId));
		criteria.add(Restrictions.ge("verrichtingsDatum", nieuweTarief.getGeldigVanafDatum()));
		if (nieuweTarief.getGeldigTotenmetDatum() != null)
		{
			criteria.add(Restrictions.lt("verrichtingsDatum", DateUtil.toUtilDate(DateUtil.toLocalDate(nieuweTarief.getGeldigTotenmetDatum()).plusDays(1))));
		}
		criteria.add(Restrictions.eq("type", tariefType));
		return criteria.list();
	}

	@Override
	public List<CervixTarief> getHuisartsTarievenTussen(Date vanafDatum, Date totEnMetDatum)
	{
		Criteria crit = getSession().createCriteria(CervixHuisartsTarief.class);
		crit.add(Restrictions.eq("actief", Boolean.TRUE));
		if (totEnMetDatum != null)
		{
			crit.add(Restrictions.lt("geldigVanafDatum", totEnMetDatum));
		}
		crit.add(Restrictions.or(Restrictions.isNull("geldigTotenmetDatum"), Restrictions.gt("geldigTotenmetDatum", vanafDatum)));

		crit.addOrder(Order.asc("geldigVanafDatum"));
		return crit.list();
	}

	@Override
	public List<CervixTarief> getLabTarievenTussen(BMHKLaboratorium laboratorium, Date vanafDatum, Date totEnMetDatum)
	{
		Criteria crit = getSession().createCriteria(CervixLabTarief.class);
		crit.add(Restrictions.eq("bmhkLaboratorium", laboratorium));
		crit.add(Restrictions.eq("actief", Boolean.TRUE));

		if (totEnMetDatum != null)
		{
			crit.add(Restrictions.lt("geldigVanafDatum", totEnMetDatum));
		}
		crit.add(Restrictions.or(Restrictions.isNull("geldigTotenmetDatum"), Restrictions.gt("geldigTotenmetDatum", vanafDatum)));

		crit.addOrder(Order.asc("geldigVanafDatum"));
		return crit.list();
	}

}
