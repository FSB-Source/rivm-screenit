package nl.rivm.screenit.main.dao.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Collections;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.main.dao.mamma.MammaBeoordelingsEenheidDao;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.apache.commons.collections.CollectionUtils;
import org.hibernate.Criteria;
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
public class MammaBeoordelingsEenheidDaoImpl extends AbstractAutowiredDao implements MammaBeoordelingsEenheidDao
{

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public long getAantalActieveGekoppeldeScreeningsEenheden(BeoordelingsEenheid beoordelingsEenheid)
	{
		Date now = currentDateSupplier.getDate();
		Criteria criteria = getSession().createCriteria(MammaScreeningsEenheid.class, "screeningsEenheid");
		criteria.createAlias("screeningsEenheid.beoordelingsEenheid", "beoordelingsEenheid");
		criteria.createAlias("screeningsEenheid.tijdelijkeBeoordelingsEenheid", "tijdelijkeBeoordelingsEenheid", JoinType.LEFT_OUTER_JOIN);
		criteria.add(Restrictions.eq("screeningsEenheid.actief", true));
		criteria.add(
			Restrictions.or(
				Restrictions.and(
					Restrictions.ge("screeningsEenheid.tijdelijkeBeTotEnMetDatum", now),
					Restrictions.eq("tijdelijkeBeoordelingsEenheid.id", beoordelingsEenheid.getId())),
				Restrictions.eq("beoordelingsEenheid.id", beoordelingsEenheid.getId())));
		criteria.setProjection(Projections.rowCount());

		return (long) criteria.uniqueResult();
	}

	@Override
	public long getAantalNietAfgerondeGekoppeldeOnderzoeken(BeoordelingsEenheid beoordelingsEenheid)
	{
		Criteria criteria = getSession().createCriteria(MammaOnderzoek.class, "onderzoek");
		criteria.createAlias("onderzoek.screeningsEenheid", "screeningsEenheid");
		criteria.createAlias("screeningsEenheid.beoordelingsEenheid", "beoordelingsEenheid");
		criteria.add(Restrictions.eq("beoordelingsEenheid.id", beoordelingsEenheid.getId()));
		criteria.add(Restrictions.eq("onderzoek.isDoorgevoerd", false));
		criteria.setProjection(Projections.rowCount());

		return (long) criteria.uniqueResult();
	}

	@Override
	public long getAantalNietAfgerondeGekoppeldeBeoordelingen(BeoordelingsEenheid beoordelingsEenheid)
	{
		Criteria criteria = getSession().createCriteria(MammaOnderzoek.class, "onderzoek");
		criteria.createAlias("onderzoek.beoordelingen", "beoordeling");
		criteria.createAlias("onderzoek.screeningsEenheid", "screeningsEenheid");
		criteria.createAlias("screeningsEenheid.beoordelingsEenheid", "beoordelingsEenheid");
		criteria.add(Restrictions.eq("beoordelingsEenheid.id", beoordelingsEenheid.getId()));
		criteria.add(Restrictions.not(Restrictions.in("beoordeling.status", MammaBeoordelingStatus.eindStatussen())));

		criteria.setProjection(Projections.rowCount());

		return (long) criteria.uniqueResult();
	}

	@Override
	public List<BeoordelingsEenheid> getActieveBeoordelingsEenhedenVoorScreeningsOrganisatie(ScreeningOrganisatie screeningOrganisatie)
	{
		if (screeningOrganisatie == null)
		{
			return Collections.emptyList();
		}

		Criteria criteria = getSession().createCriteria(BeoordelingsEenheid.class, "be");
		criteria.createAlias("be.parent", "ce");
		criteria.createAlias("ce.regio", "so");
		criteria.add(Restrictions.eq("actief", true));
		criteria.addOrder(Order.asc("naam"));

		criteria.add(Restrictions.eq("so.id", screeningOrganisatie.getId()));

		return criteria.list();
	}

	@Override
	public List<BeoordelingsEenheid> getActieveBeoordelingsEenhedenVoorCentraleEenheden(List<CentraleEenheid> centraleEenheden)
	{
		if (CollectionUtils.isEmpty(centraleEenheden))
		{
			return Collections.emptyList();
		}

		Criteria criteria = getSession().createCriteria(BeoordelingsEenheid.class, "be");
		criteria.createAlias("be.parent", "ce");
		criteria.add(Restrictions.eq("actief", true));
		criteria.addOrder(Order.asc("naam"));

		criteria.add(Restrictions.in("ce.id", centraleEenheden.stream().map(ce -> ce.getId()).toArray()));
		return criteria.list();

	}
}
