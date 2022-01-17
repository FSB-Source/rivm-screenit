package nl.rivm.screenit.batch.jobs.colon.huisartsberichten.step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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
import nl.rivm.screenit.model.colon.ColonHuisartsBericht;
import nl.rivm.screenit.model.colon.ColonHuisartsBerichtStatus;

import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.springframework.beans.factory.annotation.Autowired;

public class ColonMislukteHuisartsberichtenVersturenReader extends BaseScrollableResultReader
{
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	private final Integer MAXIMALE_PERIODE_RETRY = 1;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{

		Criteria criteria = session.createCriteria(ColonHuisartsBericht.class, "huisartsbericht");
		criteria.add(Restrictions.eq("huisartsbericht.status", ColonHuisartsBerichtStatus.VERZENDEN_MISLUKT));
		criteria.add(Restrictions.ge("huisartsbericht.aanmaakDatum", DateUtil.toUtilDate(currentDateSupplier.getLocalDateTime().minusMonths(MAXIMALE_PERIODE_RETRY))));

		return criteria;
	}
}
