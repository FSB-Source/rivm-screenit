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

import java.time.LocalDate;
import java.util.List;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.mamma.MammaBaseTehuisDao;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.MammaTehuis;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.Criteria;
import org.hibernate.criterion.MatchMode;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaBaseTehuisDaoImpl extends AbstractAutowiredDao implements MammaBaseTehuisDao
{
	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	private Criteria createCriteria(MammaTehuis zoekTehuis, ScreeningOrganisatie screeningOrganisatie)
	{
		Criteria criteria = getSession().createCriteria(MammaTehuis.class);
		criteria.createAlias("standplaats", "standplaats");
		criteria.createAlias("standplaats.regio", "regio");

		if (StringUtils.isNotBlank(zoekTehuis.getNaam()))
		{
			criteria.add(Restrictions.ilike("naam", zoekTehuis.getNaam(), MatchMode.ANYWHERE));
		}
		if (zoekTehuis.getStandplaats() != null)
		{
			criteria.add(Restrictions.eq("standplaats.id", zoekTehuis.getStandplaats().getId()));
		}
		if (screeningOrganisatie != null)
		{
			criteria.add(Restrictions.eq("regio.id", screeningOrganisatie.getId()));
		}
		if (zoekTehuis.getActief() != null)
		{
			criteria.add(Restrictions.eq("actief", zoekTehuis.getActief()));
		}

		return criteria;
	}

	@Override
	public List<MammaTehuis> zoekTehuizen(MammaTehuis tehuis, ScreeningOrganisatie screeningOrganisatie, int first, int count, String sortProperty, boolean asc)
	{
		Criteria crit = createCriteria(tehuis, screeningOrganisatie);
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
		if (first >= 0)
		{
			crit.setFirstResult(first);
		}
		if (count > 0)
		{
			crit.setMaxResults(count);
		}
		return crit.list();
	}

	@Override
	public long countTehuizen(MammaTehuis tehuis, ScreeningOrganisatie screeningOrganisatie)
	{
		Criteria crit = createCriteria(tehuis, screeningOrganisatie);
		crit.setProjection(Projections.rowCount());
		return ((Number) crit.uniqueResult()).longValue();
	}

	@Override
	public MammaStandplaatsRonde getHuidigeStandplaatsRonde(MammaStandplaats standplaats)
	{

		Criteria crit = getSession().createCriteria(MammaStandplaatsPeriode.class, "standplaatsPeriode");
		crit.createAlias("standplaatsPeriode.standplaatsRonde", "standplaatsRonde");
		crit.createAlias("standplaatsRonde.standplaats", "standplaats");

		Integer afspraakVanafAantalWerkdagen = preferenceService.getInteger(PreferenceKey.MAMMA_AFSPRAAK_BIJ_UITNODIGEN_VANAF_AANTAL_WERKDAGEN.name());
		LocalDate afsprakenVanafDatum = DateUtil.plusWerkdagen(dateSupplier.getLocalDate(), afspraakVanafAantalWerkdagen);
		crit.add(Restrictions.ge("standplaatsPeriode.totEnMet", DateUtil.toUtilDate(afsprakenVanafDatum)));
		crit.add(Restrictions.eq("standplaats.id", standplaats.getId()));

		crit.addOrder(Order.asc("standplaatsPeriode.vanaf"));
		crit.setMaxResults(1);
		if (crit.list().size() > 0)
		{
			MammaStandplaatsPeriode standplaatsPeriode = (MammaStandplaatsPeriode) crit.list().get(0);
			return standplaatsPeriode.getStandplaatsRonde();
		}
		return null;
	}
}
