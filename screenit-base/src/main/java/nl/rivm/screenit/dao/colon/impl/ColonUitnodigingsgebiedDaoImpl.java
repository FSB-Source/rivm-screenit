package nl.rivm.screenit.dao.colon.impl;

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
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import nl.rivm.screenit.dao.colon.ColonUitnodigingsgebiedDao;
import nl.rivm.screenit.model.PostcodeGebied;
import nl.rivm.screenit.model.UitnodigingsGebied;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.query.DateRestrictions;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.LogicalExpression;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class ColonUitnodigingsgebiedDaoImpl extends AbstractAutowiredDao implements ColonUitnodigingsgebiedDao
{
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public List<PostcodeGebied> findOverlappendePostcodeGebieden(PostcodeGebied postcode)
	{
		Criteria crit = this.getSession().createCriteria(PostcodeGebied.class);

		LogicalExpression vanPostcode = Restrictions.and(Restrictions.le("vanPostcode", postcode.getVanPostcode().toUpperCase()),
			Restrictions.ge("totPostcode", postcode.getVanPostcode().toUpperCase()));

		LogicalExpression totPostcode = Restrictions.and(Restrictions.le("vanPostcode", postcode.getTotPostcode().toUpperCase()),
			Restrictions.ge("totPostcode", postcode.getTotPostcode().toUpperCase()));

		crit.add(Restrictions.or(vanPostcode, totPostcode));

		if (postcode.getId() != null)
		{
			crit.add(Restrictions.ne("id", postcode.getId()));
		}

		return crit.list();
	}

	@Override
	public long countPersonenInUitnodigingsGebied(UitnodigingsGebied uitnodigingsGebied, Integer minimaleLeeftijd, Integer maximaleLeeftijd, Integer uitnodigingsInterval,
		LocalDate laatsteDagVanHuidigJaar, Set<Integer> geboortejaren)
	{
		Criteria crit = ColonRestrictions.getBaseCriteria(getSession(), uitnodigingsGebied, minimaleLeeftijd, maximaleLeeftijd, laatsteDagVanHuidigJaar);

		if (laatsteDagVanHuidigJaar != null)
		{
			crit.createAlias("colonDossier", "dossier", JoinType.LEFT_OUTER_JOIN);
			crit.createAlias("dossier.laatsteScreeningRonde", "laatsteScreeningRonde", JoinType.LEFT_OUTER_JOIN);
			crit.createAlias("dossier.volgendeUitnodiging", "volgendeUitnodiging", JoinType.LEFT_OUTER_JOIN);
			crit.createAlias("volgendeUitnodiging.interval", "interval", JoinType.LEFT_OUTER_JOIN);
			crit.createAlias("laatsteScreeningRonde.laatsteAfspraak", "afspraak", JoinType.LEFT_OUTER_JOIN);

			crit.add( 
				Restrictions.or(
					ColonRestrictions.getU1BaseCriteria(laatsteDagVanHuidigJaar, new ArrayList<>(geboortejaren), currentDateSupplier.getLocalDate()), 
					ColonRestrictions.getU2BaseCriteria(laatsteDagVanHuidigJaar, currentDateSupplier.getLocalDate())) 
			);
		}

		crit.setProjection(Projections.rowCount());

		return (Long) crit.uniqueResult();
	}

	@Override
	public long countPersonenInUitnodigingsGebied(UitnodigingsGebied uitnodigingsGebied)
	{
		return countPersonenInUitnodigingsGebied(uitnodigingsGebied, null, null, null, null, null);
	}

	@Override
	public long countClientenInUitnodigingsgebiedMetUitnodigingOpDatum(UitnodigingsGebied uitnodigingsGebied, LocalDate uitnodigingsDatum)
	{
		Criteria crit = ColonRestrictions.getBaseCriteria(getSession(), uitnodigingsGebied, null, null, null);

		crit.createAlias("colonDossier", "dossier");
		crit.createAlias("dossier.laatsteScreeningRonde", "laatsteScreeningRonde");

		crit.add(DateRestrictions.eq("laatsteScreeningRonde.creatieDatum", DateUtil.toUtilDate(uitnodigingsDatum)));

		return crit.list().size();
	}
}
