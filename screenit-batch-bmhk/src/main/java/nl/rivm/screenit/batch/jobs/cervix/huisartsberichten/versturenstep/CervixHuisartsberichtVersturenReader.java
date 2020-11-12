package nl.rivm.screenit.batch.jobs.cervix.huisartsberichten.versturenstep;

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

import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.cervix.CervixHuisartsBericht;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsBerichtStatus;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;

public class CervixHuisartsberichtVersturenReader extends BaseScrollableResultReader
{
	
	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		Criteria crit = session.createCriteria(CervixHuisartsBericht.class, "huisartsbericht");

		crit.add(Restrictions.or(
			Restrictions.eq("huisartsbericht.status", CervixHuisartsBerichtStatus.AANGEMAAKT),
			Restrictions.eq("huisartsbericht.status", CervixHuisartsBerichtStatus.VERSTUREN_MISLUKT),
			Restrictions.eq("huisartsbericht.status", CervixHuisartsBerichtStatus.KLANTNUMMER_NIET_GEVERIFIEERD),
			Restrictions.eq("huisartsbericht.status", CervixHuisartsBerichtStatus.OPNIEUW_VERSTUREN_MISLUKT)));
		crit.add(Restrictions.isNotNull("huisartsbericht.huisartsLocatie"));

		crit.createAlias("huisartsbericht.client", "client");
		crit.createAlias("client.persoon", "persoon");
		crit.createAlias("persoon.gbaAdres", "gbaAdres");
		crit.createAlias("gbaAdres.gbaGemeente", "gemeente");
		crit.createAlias("huisartsbericht.uitstrijkje", "uitstrijkje", JoinType.LEFT_OUTER_JOIN);
		crit.createAlias("uitstrijkje.brief", "brief", JoinType.LEFT_OUTER_JOIN);
		crit.createAlias("brief.uitnodiging", "uitnodiging", JoinType.LEFT_OUTER_JOIN);
		crit.add(Restrictions.or(

			Restrictions.isNull("uitstrijkje.id"),

			Restrictions.isNull("uitnodiging.id"),
			Restrictions.isNotNull("gemeente.bmhkLaboratorium")));

		return crit;
	}
}
