package nl.rivm.screenit.batch.jobs.cervix.verrichtingen.mainstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsBerichtStatus;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;

public class CervixBepalenVerrichtingenReader extends BaseScrollableResultReader
{
	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		Criteria criteria = session.createCriteria(CervixMonster.class, "monster");
		criteria.createAlias("monster.huisartsBericht", "huisartsbericht", JoinType.LEFT_OUTER_JOIN);
		criteria.add(Restrictions.or(Restrictions.isNull("huisartsbericht.status"), Restrictions.ne("huisartsbericht.status", CervixHuisartsBerichtStatus.AANGEMAAKT)));
		criteria.add(Restrictions.or(Restrictions.isNull("uitstrijkjeStatus"), Restrictions.ne("uitstrijkjeStatus", CervixUitstrijkjeStatus.NIET_ONTVANGEN)));
		criteria.add(Restrictions.isNotNull("monster.brief"));
		criteria.add(Restrictions.isEmpty("verrichtingen"));
		return criteria;
	}
}
