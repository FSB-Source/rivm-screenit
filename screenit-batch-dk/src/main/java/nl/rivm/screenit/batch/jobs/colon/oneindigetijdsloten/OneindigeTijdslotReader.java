package nl.rivm.screenit.batch.jobs.colon.oneindigetijdsloten;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projection;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;

@AllArgsConstructor(access = AccessLevel.PROTECTED)
public abstract class OneindigeTijdslotReader<T extends AbstractAppointment> extends BaseScrollableResultReader
{
	private final Class<T> type;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var criteria = session.createCriteria(type);

		criteria.add(Restrictions.isNotNull("recurrence"));

		criteria.createAlias("recurrence", "r");
		criteria.add(Restrictions.isNull("r.endDate"));

		var subQuery = DetachedCriteria.forClass(type);

		subQuery.createAlias("recurrence", "subRecurrence");
		subQuery.add(Restrictions.eqProperty("r.id", "subRecurrence.id"));

		subQuery.setProjection(Projections.max("startTime"));
		criteria.add(Subqueries.propertyEq("startTime", subQuery));

		criteria.addOrder(Order.asc("id"));

		return criteria;
	}

	@Override
	protected Projection getProjection()
	{
		return Projections.id();
	}
}
