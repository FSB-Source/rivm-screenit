package nl.rivm.screenit.batch.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.batch.dao.CervixUitnodigingsDao;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.util.query.ScreenitRestrictions;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class CervixUitnodigingsDaoImpl extends AbstractAutowiredDao implements CervixUitnodigingsDao
{
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private InstellingService instellingService;

	@Override
	public List<Long> getTeVersturenZasUitnodigingen()
	{
		Criteria criteria = getSession().createCriteria(CervixUitnodiging.class);
		criteria.createAlias("screeningRonde", "screeningRonde");
		criteria.createAlias("screeningRonde.dossier", "dossier");
		criteria.createAlias("dossier.client", "client");
		criteria.createAlias("client.persoon", "persoon");

		criteria.createAlias("persoon.gbaAdres", "gbaAdres");
		criteria.createAlias("gbaAdres.gbaGemeente", "gemeente");
		criteria.add(Restrictions.isNotNull("gemeente.bmhkLaboratorium"));

		ScreenitRestrictions.addClientBaseRestrictions(criteria, "client", "persoon");

		criteria.add(Restrictions.eq("screeningRonde.status", ScreeningRondeStatus.LOPEND));
		criteria.add(Restrictions.eq("verstuurd", false));
		criteria.add(Restrictions.le("uitnodigingsDatum", currentDateSupplier.getDate()));
		criteria.add(Restrictions.eq("monsterType", CervixMonsterType.ZAS));
		criteria.add(Restrictions.isNull("geannuleerdDatum"));

		DetachedCriteria subQuery = DetachedCriteria.forClass(CervixDossier.class);
		subQuery.createAlias("laatsteScreeningRonde", "ronde");
		subQuery.createAlias("ronde.uitnodigingen", "uitnodigingen");
		subQuery.createAlias("uitnodigingen.brief", "brief");
		subQuery.createAlias("brief.mergedBrieven", "mergedbrief");
		subQuery.add(Restrictions.eq("mergedbrief.geprint", true));
		subQuery.add(Restrictions.eq("brief.briefType", BriefType.CERVIX_UITNODIGING));
		subQuery.setProjection(Projections.distinct(Projections.property("ronde.id")));

		criteria.add(Subqueries.propertyIn("screeningRonde.id", subQuery));
		Integer maxAantalZasUitnodigingen = getMaxAantalZasUitnodigingen();
		if (maxAantalZasUitnodigingen != null)
		{
			criteria.setMaxResults(maxAantalZasUitnodigingen);
			criteria.addOrder(Order.asc("uitnodigingsDatum"));
			criteria.setProjection(Projections.projectionList().add(Projections.id()).add(Projections.property("uitnodigingsDatum")));
			List<Object[]> resultaten = criteria.list();
			return resultaten.stream().map(rij -> (Long) rij[0]).collect(Collectors.toList());
		}
		else
		{
			criteria.setProjection(Projections.id());
			return criteria.list();
		}

	}

	private Integer getMaxAantalZasUitnodigingen()
	{
		return instellingService.getOrganisatieParameter(null, OrganisatieParameterKey.CERVIX_MAX_AANTAL_ZAS_NAAR_INPAKCENTRUM);
	}

}
