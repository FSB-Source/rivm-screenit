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

import java.lang.reflect.ParameterizedType;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;

import nl.rivm.screenit.util.functionalinterfaces.TriFunction;
import nl.topicuszorg.hibernate.object.model.HibernateObject;

import org.hibernate.Query;
import org.hibernate.ScrollMode;
import org.hibernate.ScrollableResults;
import org.hibernate.StatelessSession;
import org.hibernate.internal.EmptyScrollableResults;
import org.springframework.data.jpa.domain.Specification;

public abstract class BaseSpecificationScrollableResultReader<T extends HibernateObject, R> extends BaseIdScrollableResultReader
{

	protected abstract Specification<T> createSpecification();

	protected CriteriaQuery<R> createProjection(Root<T> r, CriteriaQuery<R> q, CriteriaBuilder cb)
	{
		return q.select(r.get("id")).distinct(true);
	}

	protected ScrollableResults createScrollableResults(TriFunction<CriteriaQuery<R>, Root<T>, CriteriaBuilder, CriteriaQuery<R>> queryConsumer)
	{
		if (getMaxResults() == 0)
		{
			return new EmptyScrollableResults();
		}

		var cb = getHibernateSession().getCriteriaBuilder();
		var query = cb.createQuery(getResultClass());
		var r = query.from(getEntityClass());

		createProjection(r, query, cb).where(createSpecification().toPredicate(r, query, cb));

		query = queryConsumer.apply(query, r, cb);

		var hquery = getHibernateSession().createQuery(query).unwrap(Query.class);

		if (getMaxResults() > 0)
		{
			hquery.setMaxResults(getMaxResults());
		}

		return hquery.setFetchSize(fetchSize).scroll(ScrollMode.FORWARD_ONLY);
	}

	@Override
	protected ScrollableResults createScrollableResults(StatelessSession session)
	{
		return createScrollableResults((query, r, cb) -> query);
	}

	protected Class<T> getEntityClass()
	{
		return (Class<T>) ((ParameterizedType) getClass().getGenericSuperclass()).getActualTypeArguments()[0];
	}

	protected Class<R> getResultClass()
	{

		return (Class<R>) ((ParameterizedType) getClass().getGenericSuperclass()).getActualTypeArguments()[1];
	}

	protected int getMaxResults()
	{
		return -1;
	}
}
