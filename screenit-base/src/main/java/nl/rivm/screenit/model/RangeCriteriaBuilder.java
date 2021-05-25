package nl.rivm.screenit.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import static org.hibernate.criterion.Restrictions.and;
import static org.hibernate.criterion.Restrictions.gt;
import static org.hibernate.criterion.Restrictions.le;
import static org.hibernate.criterion.Restrictions.lt;

import java.util.Date;

import org.hibernate.criterion.Criterion;
import org.joda.time.DateTime;
import org.joda.time.Interval;

public abstract class RangeCriteriaBuilder
{
	public abstract Criterion contains(DateTime date);

	public abstract Criterion overlaps(String startProperty, String endProperty);

	public abstract Criterion overlaps(Interval interval);

	public Criterion overlaps(Date startDate, Date endDate)
	{
		return overlaps(new Interval(new DateTime(startDate), new DateTime(endDate)));
	}

	public static RangeCriteriaBuilder closedOpen(final String closedStartProperty, final String openEndProperty)
	{
		return new RangeCriteriaBuilder()
		{
			@Override
			public Criterion contains(DateTime date)
			{
				return and(le(closedStartProperty, date), gt(openEndProperty, date));
			}

			@Override
			public Criterion overlaps(String startProperty, String endProperty)
			{
				return and(gt(openEndProperty, endProperty), lt(closedStartProperty, endProperty));
			}

			@Override
			public Criterion overlaps(Interval interval)
			{
				return and(gt(openEndProperty, interval.getStart().toDate()), lt(closedStartProperty, interval.getEnd().toDate()));
			}
		};
	}
}
