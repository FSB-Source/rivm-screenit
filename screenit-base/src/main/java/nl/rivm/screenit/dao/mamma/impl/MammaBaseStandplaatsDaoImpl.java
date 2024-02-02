package nl.rivm.screenit.dao.mamma.impl;

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

import java.util.Date;
import java.util.List;

import nl.rivm.screenit.dao.mamma.MammaBaseStandplaatsDao;
import nl.rivm.screenit.dto.mamma.afspraken.IMammaAfspraakWijzigenFilter;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.MammaStandplaatsOpmerking;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.Criteria;
import org.hibernate.criterion.MatchMode;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaBaseStandplaatsDaoImpl extends AbstractAutowiredDao implements MammaBaseStandplaatsDao
{

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public List<MammaStandplaats> zoekStandplaatsen(MammaStandplaats zoekObject, int first, int count, String sortProperty, boolean asc)
	{
		Criteria crit = createCriteria(zoekObject);
		if (sortProperty != null)
		{
			if (asc)
			{
				crit.addOrder(Order.asc(sortProperty));
			}
			else
			{
				crit.addOrder(Order.desc(sortProperty));
			}
		}
		crit.setFirstResult(Math.max(first, 0));
		if (count < 0)
		{
			crit.setMaxResults(Integer.MAX_VALUE);
		}
		else
		{
			crit.setMaxResults(count);
		}
		return crit.list();
	}

	private Criteria createCriteria(MammaStandplaats zoekObject)
	{
		Criteria criteria = getSession().createCriteria(MammaStandplaats.class);

		criteria.createAlias("locatie", "locatie");
		criteria.createAlias("regio", "regio");
		if (StringUtils.isNotBlank(zoekObject.getNaam()))
		{
			criteria.add(Restrictions.ilike("naam", zoekObject.getNaam(), MatchMode.ANYWHERE));
		}
		if (StringUtils.isNotBlank(zoekObject.getLocatie().getPlaats()))
		{
			criteria.createAlias("tijdelijkeLocatie", "tijdelijkeLocatie");
			criteria.add(Restrictions.or(Restrictions.ilike("locatie.plaats", zoekObject.getLocatie().getPlaats(), MatchMode.ANYWHERE),
				Restrictions.ilike("tijdelijkeLocatie.plaats", zoekObject.getLocatie().getPlaats(), MatchMode.ANYWHERE)));
		}
		if (zoekObject.getRegio() != null)
		{
			criteria.add(Restrictions.eq("regio.id", zoekObject.getRegio().getId()));
		}
		if (zoekObject.getActief() != null)
		{
			criteria.add(Restrictions.eq("actief", zoekObject.getActief()));
		}
		return criteria;
	}

	@Override
	public long countStandplaatsen(MammaStandplaats zoekObject)
	{
		Criteria crit = createCriteria(zoekObject);
		crit.setProjection(Projections.rowCount());
		return (Long) crit.uniqueResult();
	}

	@Override
	public List<MammaStandplaats> getActieveStandplaatsen(ScreeningOrganisatie voorRegio)
	{
		MammaStandplaats zoekObject = new MammaStandplaats();
		zoekObject.setLocatie(new MammaStandplaatsLocatie());
		zoekObject.setRegio(voorRegio);
		zoekObject.setActief(true);
		return createCriteria(zoekObject).addOrder(Order.asc("naam")).list();
	}

	@Override
	public boolean heeftActieveOpmerking(MammaStandplaats standplaats)
	{
		Criteria crit = getSession().createCriteria(MammaStandplaatsOpmerking.class, "standplaatsOpmerking");
		crit.add(Restrictions.eq("standplaatsOpmerking.standplaats.id", standplaats.getId()));
		crit.add(Restrictions.eq("standplaatsOpmerking.actief", true));
		crit.setProjection(Projections.rowCount());
		return (Long) crit.uniqueResult() > 0;
	}

	@Override
	public boolean heeftStandplaatsRondenBijScreeningsRonden(MammaStandplaatsRonde ronde)
	{
		Criteria crit = getSession().createCriteria(MammaScreeningRonde.class);
		crit.add(Restrictions.eq("standplaatsRonde.id", ronde.getId()));
		crit.setProjection(Projections.rowCount());
		return (Long) crit.uniqueResult() > 0;
	}

	@Override
	public boolean heeftAfspraken(MammaStandplaatsRonde ronde)
	{
		Criteria crit = getSession().createCriteria(MammaAfspraak.class);
		crit.createAlias("standplaatsPeriode", "standplaatsPeriode");
		crit.createAlias("standplaatsPeriode.standplaatsRonde", "standplaatsRonde");
		crit.add(Restrictions.eq("standplaatsRonde.id", ronde.getId()));
		crit.setProjection(Projections.rowCount());
		return (Long) crit.uniqueResult() > 0;
	}

	@Override
	public boolean magStandplaatsInactiveren(MammaStandplaats standplaats, Date vandaag)
	{

		Criteria aantalUitgenodigdCrit = getSession().createCriteria(MammaScreeningRonde.class);
		aantalUitgenodigdCrit.createAlias("standplaatsRonde", "standplaatsRonde");
		aantalUitgenodigdCrit.createAlias("standplaatsRonde.standplaatsPerioden", "periode");
		aantalUitgenodigdCrit.createAlias("standplaatsRonde.standplaats", "standplaats");
		aantalUitgenodigdCrit.add(Restrictions.eq("standplaats.id", standplaats.getId()));
		aantalUitgenodigdCrit.add(Restrictions.ge("periode.totEnMet", vandaag));
		aantalUitgenodigdCrit.setProjection(Projections.count("standplaatsRonde.id"));

		Long aantalUitgenodigd = (Long) aantalUitgenodigdCrit.uniqueResult();
		if (aantalUitgenodigd != 0)
		{
			return false;
		}

		Criteria aantalAfspraakCrit = getSession().createCriteria(MammaAfspraak.class);
		aantalAfspraakCrit.createAlias("standplaatsPeriode", "standplaatsPeriode");
		aantalAfspraakCrit.createAlias("standplaatsPeriode.standplaatsRonde", "standplaatsRonde");
		aantalAfspraakCrit.createAlias("standplaatsRonde.standplaats", "standplaats");
		aantalAfspraakCrit.add(Restrictions.eq("standplaats.id", standplaats.getId()));
		aantalAfspraakCrit.add(Restrictions.ge("standplaatsPeriode.totEnMet", vandaag));
		aantalAfspraakCrit.setProjection(Projections.rowCount());

		return (Long) aantalAfspraakCrit.uniqueResult() == 0;
	}

	@Override
	public MammaStandplaats getStandplaatsMetPostcode(String postcode)
	{
		Criteria crit = getSession().createCriteria(MammaStandplaats.class);
		crit.createAlias("postcodeReeksen", "postcodeReeks");
		crit.add(Restrictions.le("postcodeReeks.vanPostcode", postcode));
		crit.add(Restrictions.ge("postcodeReeks.totPostcode", postcode));
		crit.add(Restrictions.eq("actief", true));

		return (MammaStandplaats) crit.uniqueResult();
	}

	@Override
	public MammaStandplaatsRonde getVorigeStandplaatsRonde(MammaStandplaatsRonde standplaatsRonde)
	{
		Date vanaf = standplaatsRonde.getStandplaatsPerioden().stream()
			.filter(standplaatsPeriode -> standplaatsPeriode.getStandplaatsRondeVolgNr() == 1).findAny().orElse(null).getVanaf();

		Criteria crit = getSession().createCriteria(MammaStandplaatsRonde.class, "standplaatsRonde");
		crit.createAlias("standplaatsRonde.standplaatsPerioden", "standplaatsPeriode",
			JoinType.INNER_JOIN, Restrictions.eq("standplaatsPeriode.standplaatsRondeVolgNr", 1));

		crit.add(Restrictions.lt("standplaatsPeriode.vanaf", vanaf));

		crit.addOrder(Order.desc("standplaatsPeriode.vanaf"));
		crit.setMaxResults(1);

		List list = crit.list();
		if (!list.isEmpty())
		{
			return (MammaStandplaatsRonde) list.get(0);
		}
		return null;
	}

	@Override
	public long countActieveStandplaatsPeriodes(MammaStandplaats standplaats)
	{
		Criteria criteria = getSession().createCriteria(MammaStandplaatsPeriode.class);
		criteria.createAlias("standplaatsRonde", "standplaatsRonde");
		criteria.createAlias("standplaatsRonde.standplaats", "standplaats");
		criteria.createAlias("screeningsEenheid", "screeningsEenheid");
		criteria.createAlias("screeningsEenheid.beoordelingsEenheid", "beoordelingsEenheid");
		criteria.createAlias("beoordelingsEenheid.parent", "centraleEenheid");
		criteria.add(Restrictions.eq("standplaats.id", standplaats.getId()));
		criteria.add(Restrictions.eq("standplaats.actief", true));
		criteria.add(Restrictions.eq("centraleEenheid.actief", true));
		criteria.add(Restrictions.ge("totEnMet", currentDateSupplier.getDate()));
		criteria.setProjection(Projections.rowCount());

		return (Long) criteria.uniqueResult();
	}

	@Override
	public List<MammaStandplaatsPeriode> getStandplaatsPerioden(IMammaAfspraakWijzigenFilter filter)
	{
		Criteria criteria = getSession().createCriteria(MammaStandplaatsPeriode.class);
		criteria.createAlias("screeningsEenheid", "screeningsEenheid");
		criteria.createAlias("standplaatsRonde", "standplaatsRonde");
		criteria.createAlias("standplaatsRonde.standplaats", "standplaats");
		criteria.createAlias("standplaats.locatie", "locatie");

		if (!filter.getScreeningsEenheden().isEmpty())
		{
			criteria.add(Restrictions.in("screeningsEenheid", filter.getScreeningsEenheden()));
		}

		criteria.add(Restrictions.le("vanaf", DateUtil.toUtilDate(filter.getTotEnMet())));
		criteria.add(Restrictions.ge("totEnMet", DateUtil.toUtilDate(filter.getVanaf())));
		criteria.add(Restrictions.eq("standplaats.actief", true));
		criteria.add(Restrictions.eq("screeningsEenheid.actief", true));

		if (StringUtils.isNotBlank(filter.getPlaats()))
		{
			criteria.add(Restrictions.eq("locatie.plaats", filter.getPlaats()));
		}
		else if (!filter.getStandplaatsen().isEmpty())
		{
			criteria.add(Restrictions.in("standplaatsRonde.standplaats", filter.getStandplaatsen()));
		}
		else if (!filter.isBuitenRegio())
		{
			criteria.createAlias("standplaatsRonde.afspraakcapaciteitBeschikbaarVoor", "afspraakcapaciteitBeschikbaarVoor", JoinType.LEFT_OUTER_JOIN);
			ScreeningOrganisatie screeningOrganisatie = filter.getClient().getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie();
			criteria.add(
				Restrictions.or(Restrictions.eq("standplaats.regio", screeningOrganisatie), Restrictions.eq("afspraakcapaciteitBeschikbaarVoor.id", screeningOrganisatie.getId())));
		}
		criteria.addOrder(Order.asc("locatie.plaats"));
		return criteria.list();
	}
}
