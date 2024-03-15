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

import java.util.List;

import nl.rivm.screenit.main.dao.mamma.MammaConclusieReviewDao;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.SQLQuery;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaConclusieReviewDaoImpl extends AbstractAutowiredDao implements MammaConclusieReviewDao
{
	@Override
	public List<InstellingGebruiker> getRadiologenMetLezingVanRondeEnZonderReview(MammaScreeningRonde screeningRonde)
	{
		StringBuilder queryString = new StringBuilder();
		queryString.append("select distinct {ig.*}");

		queryString.append(" from algemeen.instelling_gebruiker ig");

		queryString.append(" inner join algemeen.org_organisatie_medewerker ig_1_ on ig.id = ig_1_.id");
		queryString.append(" inner join mamma.lezing l on ig.id = l.beoordelaar");
		queryString.append(" inner join mamma.beoordeling b on l.id = b.eerste_lezing or l.id = b.tweede_lezing or l.id = b.arbitrage_lezing or l.id = b.discrepantie_lezing");
		queryString.append(" inner join mamma.onderzoek o on b.onderzoek = o.id");
		queryString.append(" inner join mamma.afspraak a on o.id = a.onderzoek");
		queryString.append(" inner join mamma.uitnodiging u on a.uitnodiging = u.id");
		queryString.append(" inner join mamma.screening_ronde sr on u.screening_ronde = sr.id");
		queryString.append(" left outer join mamma.conclusie_review cr on sr.id = cr.screening_ronde and cr.radioloog = ig.id");
		queryString.append(" where sr.id = :screeningRondeId");
		queryString.append(" and cr.id is null");

		SQLQuery query = getSession().createSQLQuery(queryString.toString()).addEntity("ig", InstellingGebruiker.class);
		query.setParameter("screeningRondeId", screeningRonde.getId());

		return (List<InstellingGebruiker>) query.list();
	}
}
