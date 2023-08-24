package nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.step.rondes;

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

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.batch.jobs.mamma.beoordeling.ilm.MammaIlmJobListener;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projection;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class MammaScreeningRondesVerwijderenReader extends BaseScrollableResultReader
{

	private final SimplePreferenceService preferenceService;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var verwijderGrensDatum = DateUtil.toUtilDate(currentDateSupplier.getLocalDate().minusDays(preferenceService.getInteger(PreferenceKey.ILM_BEWAARTERMIJN.name())));

		var crit = session.createCriteria(MammaScreeningRonde.class, "ronde");

		crit.add(Restrictions.eq("ronde.status", ScreeningRondeStatus.AFGEROND));
		crit.add(Restrictions.le("ronde.statusDatum", verwijderGrensDatum));
		crit.add(Restrictions.gt("ronde.id", getExecutionContext().get(MammaIlmJobListener.KEY_LAATSTE_RONDE_ID)));
		crit.setMaxResults(MammaIlmJobListener.MAX_AANTAL_RONDES_VERWERKEN_IN_STEP);
		crit.addOrder(Order.asc("ronde.id"));
		return crit;
	}

	@Override
	protected Projection getProjection()
	{
		return Projections.id();
	}
}
