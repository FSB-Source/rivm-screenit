package nl.rivm.screenit.batch.jobs.mamma.uitnodigen.uitstel;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.dao.mamma.MammaSelectieRestrictions;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaUitstelReden;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.query.Limit;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.hibernate.sql.JoinType;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class MammaUitstelUitnodigenReader extends BaseScrollableResultReader
{
	private final MammaSelectieRestrictions selectieRestricties;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var vrijgegevenTotEnMetSubquery = getVrijgegevenTotEnMetSubquery();
		var maximaleStreefDatumIndienStandplaatsNietInEenRoute = DateUtil.toUtilDate(currentDateSupplier.getLocalDate().plusWeeks(8));

		var criteria = session.createCriteria(Client.class, "client");
		criteria.createAlias("client.persoon", "persoon");
		criteria.createAlias("persoon.gbaAdres", "adres");
		criteria.createAlias("persoon.tijdelijkGbaAdres", "tijdelijkGbaAdres", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("client.mammaDossier", "dossier");
		criteria.createAlias("dossier.laatsteScreeningRonde", "laatsteScreeningRonde");
		criteria.createAlias("laatsteScreeningRonde.laatsteUitstel", "laatsteUitstel");
		criteria.add(Restrictions.eq("laatsteScreeningRonde.status", ScreeningRondeStatus.LOPEND));
		criteria.add(Restrictions.isNull("laatsteUitstel.geannuleerdOp"));
		criteria.add(Restrictions.isNull("laatsteUitstel.uitnodiging"));
		criteria.add(Restrictions.eq("laatsteUitstel.uitstelReden", MammaUitstelReden.CLIENT_CONTACT));
		criteria.add(Restrictions.or(
			Subqueries.propertyLe("laatsteUitstel.streefDatum", vrijgegevenTotEnMetSubquery),
			Restrictions.and(
				Restrictions.le("laatsteUitstel.streefDatum", maximaleStreefDatumIndienStandplaatsNietInEenRoute),
				Subqueries.notExists(vrijgegevenTotEnMetSubquery))));

		selectieRestricties.addStandaardSelectieRestricties(criteria);

		return criteria;
	}

	private DetachedCriteria getVrijgegevenTotEnMetSubquery()
	{
		var subquery = DetachedCriteria.forClass(MammaScreeningsEenheid.class, "screeningsEenheid");
		subquery.createAlias("screeningsEenheid.standplaatsPerioden", "standplaatsperiode");
		subquery.createAlias("standplaatsperiode.standplaatsRonde", "standplaatsRonde");
		subquery.createAlias("standplaatsRonde.standplaats", "standplaats");
		subquery.add(Restrictions.eq("screeningsEenheid.actief", true));
		subquery.add(Restrictions.eq("standplaats.actief", true));
		subquery.add(Restrictions.ge("standplaatsperiode.totEnMet", currentDateSupplier.getDateMidnight()));
		subquery.add(Restrictions.eqProperty("standplaats.id", "laatsteUitstel.standplaats"));
		subquery.setProjection(Projections.property("screeningsEenheid.vrijgegevenTotEnMet"));
		subquery.addOrder(Order.asc("standplaatsperiode.vanaf"));
		subquery.addOrder(Limit.by(1));
		return subquery;
	}
}
