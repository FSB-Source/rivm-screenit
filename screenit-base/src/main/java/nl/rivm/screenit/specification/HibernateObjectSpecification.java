package nl.rivm.screenit.specification;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Collection;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;
import nl.topicuszorg.hibernate.object.model.HibernateObject;

import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNullExtended;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class HibernateObjectSpecification
{

	public static <T extends HibernateObject> ExtendedSpecification<T> heeftId(Long id)
	{
		return (r, q, cb) -> cb.equal(r.get(AbstractHibernateObject_.ID), id);
	}

	public static <T extends HibernateObject> ExtendedSpecification<T> filterId(Long id)
	{
		return skipWhenNullExtended(id, heeftId(id));
	}

	public static <T extends HibernateObject> ExtendedSpecification<T> heeftNietId(Long id)
	{
		return (r, q, cb) -> cb.notEqual(r.get(AbstractHibernateObject_.ID), id);
	}

	public static <T extends HibernateObject> ExtendedSpecification<T> filterNietId(Long id)
	{
		return skipWhenNullExtended(id, heeftNietId(id));
	}

	public static <T extends HibernateObject> ExtendedSpecification<T> heeftIdIn(Collection<Long> ids)
	{
		return (r, q, cb) -> r.get(AbstractHibernateObject_.ID).in(ids);
	}

	public static <T extends HibernateObject> ExtendedSpecification<T> heeftNietIdIn(Collection<Long> ids)
	{
		return (r, q, cb) -> cb.not(r.get(AbstractHibernateObject_.ID).in(ids));
	}

	public static <T extends HibernateObject> ExtendedSpecification<T> heeftGeenId()
	{
		return (r, q, cb) -> cb.isNull(r.get(AbstractHibernateObject_.ID));
	}

	public static <T extends HibernateObject> ExtendedSpecification<T> heeftId()
	{
		return (r, q, cb) -> cb.isNotNull(r.get(AbstractHibernateObject_.ID));
	}

	public static <S extends HibernateObject> ExtendedSpecification<S> heeftIdGroterDan(Long id)
	{
		return (r, q, cb) -> cb.greaterThan(r.get(AbstractHibernateObject_.ID), id);
	}
}
