package nl.rivm.screenit.batch.jobs.cervix.hpvoru;

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

import static nl.rivm.screenit.batch.jobs.cervix.hpvoru.CervixHpvOruBerichtenConstants.KEY_LABORATORIUMID;

import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.cervix.enums.CervixHpvUitslag;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;

public class CervixHpvOruBerichtenReader extends BaseScrollableResultReader
{
	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		Criteria criteria = session.createCriteria(CervixScreeningRonde.class);
		criteria.add(Restrictions.isNotNull("monsterHpvUitslag"));
		criteria.createAlias("monsterHpvUitslag", "monster");
		criteria.createAlias("monster.laboratorium", "laboratorium");
		criteria.createAlias("monster.laatsteHpvBeoordeling", "laatsteHpvBeoordeling");
		criteria.add(Restrictions.eq("laboratorium.actief", Boolean.TRUE));
		criteria.add(Restrictions.eq("laboratorium.oruBerichtenVerwerken", Boolean.TRUE));
		criteria.add(Restrictions.isNotNull("monster.brief"));
		criteria.add(Restrictions.or(
			Restrictions.eq("laatsteHpvBeoordeling.hpvUitslag", CervixHpvUitslag.NEGATIEF),
			Restrictions.eq("monster.class", CervixZas.class)));
		criteria.add(Restrictions.isNull("monster.datumOruVerstuurd"));
		criteria.add(Restrictions.eq("laboratorium.id", getStepExecutionContext().getLong(KEY_LABORATORIUMID)));
		return criteria;
	}
}
