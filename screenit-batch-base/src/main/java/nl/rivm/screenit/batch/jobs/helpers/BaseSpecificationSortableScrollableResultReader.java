package nl.rivm.screenit.batch.jobs.helpers;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.util.ArrayList;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Selection;

import nl.topicuszorg.hibernate.object.model.HibernateObject;

import org.hibernate.ScrollableResults;
import org.hibernate.StatelessSession;

public abstract class BaseSpecificationSortableScrollableResultReader<T extends HibernateObject, R> extends BaseSpecificationScrollableResultReader<T, R>
{
	protected abstract Order getOrder(Root<T> r, CriteriaBuilder cb);

	@Override
	protected ScrollableResults createScrollableResults(StatelessSession session)
	{
		return super.createScrollableResults((query, r, cb) ->
		{
			var order = getOrder(r, cb);
			if (order != null)
			{
				var selection = new ArrayList<Selection<?>>();
				if (query.getSelection().isCompoundSelection())
				{
					selection.addAll(query.getSelection().getCompoundSelectionItems());
				}
				else
				{
					selection.add(query.getSelection());
				}
				selection.add(order.getExpression());
				query.multiselect(selection).distinct(true);
				query.orderBy(order);
			}
			return query;
		});
	}

	@Override
	protected Long getScrollableResult(ScrollableResults scrollableResults)
	{
		var scrollableResult = scrollableResults.get(0);
		return scrollableResult instanceof Long ? (Long) scrollableResult : (Long) ((Object[]) scrollableResults.get()[0])[0];
	}
}
