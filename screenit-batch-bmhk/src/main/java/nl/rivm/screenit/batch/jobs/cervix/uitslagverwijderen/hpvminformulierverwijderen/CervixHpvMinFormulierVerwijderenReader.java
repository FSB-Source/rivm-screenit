package nl.rivm.screenit.batch.jobs.cervix.uitslagverwijderen.hpvminformulierverwijderen;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.enums.CervixHpvBeoordelingWaarde;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Component;

@Component
public class CervixHpvMinFormulierVerwijderenReader extends BaseScrollableResultReader
{
	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var criteria = session.createCriteria(CervixLabformulier.class);

		criteria.createAlias("uitstrijkje", "uitstrijkje");
		criteria.createAlias("uitstrijkje.ontvangstScreeningRonde", "ontvangstRonde");

		criteria.createAlias("ontvangstRonde.monsterHpvUitslag", "monsterHpvUitslag");
		criteria.createAlias("monsterHpvUitslag.laatsteHpvBeoordeling", "laatsteHpvBeoordeling");

		criteria.add(Restrictions.eq("laatsteHpvBeoordeling.hpvUitslag", CervixHpvBeoordelingWaarde.NEGATIEF));

		criteria.add(Restrictions.eq("ontvangstRonde.status", ScreeningRondeStatus.AFGEROND));
		criteria.add(Restrictions.eq("ontvangstRonde.aangemeld", Boolean.TRUE));

		criteria.add(Restrictions.isNull("datumGewist"));
		criteria.add(Restrictions.isNotNull("uitstrijkje.brief"));

		return criteria;
	}
}
