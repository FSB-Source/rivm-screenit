package nl.rivm.screenit.batch.jobs.mamma.huisartsberichten.step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import java.time.LocalDateTime;
import java.util.Date;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.mamma.berichten.MammaHuisartsBericht;
import nl.rivm.screenit.model.mamma.enums.MammaHuisartsBerichtStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.springframework.beans.factory.annotation.Autowired;

public class MammaMislukteHuisartsberichtenVersturenReader extends BaseScrollableResultReader
{
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{

		final LocalDateTime huisartsBerichtWachtTijd = currentDateSupplier.getLocalDateTime().minusMinutes(Constants.BK_HA_BATCH_BERICHTEN_AANMAAK_OUDER_DAN);
		final Date huisartsberichtWachttijdDate = DateUtil.toUtilDate(huisartsBerichtWachtTijd);
		Criteria criteria = session.createCriteria(MammaHuisartsBericht.class, "huisartsbericht");
		criteria.add(Restrictions.or(
			Restrictions.eq("huisartsbericht.status", MammaHuisartsBerichtStatus.VERSTUREN_MISLUKT),
			Restrictions.and(

				Restrictions.eq("huisartsbericht.status", MammaHuisartsBerichtStatus.AANGEMAAKT),
				Restrictions.le("huisartsbericht.statusDatum", huisartsberichtWachttijdDate))));

		return criteria;
	}
}
