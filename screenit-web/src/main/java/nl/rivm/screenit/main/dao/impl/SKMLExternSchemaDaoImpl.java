package nl.rivm.screenit.main.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.main.dao.SKMLExternSchemaDao;
import nl.rivm.screenit.model.colon.SKMLExternSchema;
import nl.rivm.screenit.model.colon.SKMLExterneControleBarcode;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.joda.time.DateTime;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class SKMLExternSchemaDaoImpl extends AbstractAutowiredDao implements SKMLExternSchemaDao
{
	@Override
	@SuppressWarnings("unchecked")
	@Transactional(propagation = Propagation.SUPPORTS)
	public Iterator<? extends SKMLExternSchema> zoekSchemas(SKMLExternSchema zoekObject, String sortProperty, boolean ascending, int first, int count)
	{
		Criteria crit = maakCriteria(zoekObject);
		if (sortProperty != null)
		{
			if (ascending)
			{
				crit.addOrder(Order.asc(sortProperty));
			}
			else
			{
				crit.addOrder(Order.desc(sortProperty));
			}
		}
		crit.setFirstResult(first);
		crit.setMaxResults(count);
		crit.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);
		return crit.list().iterator();
	}

	@Override
	@SuppressWarnings("unchecked")
	@Transactional(propagation = Propagation.SUPPORTS)
	public long telSchemas(SKMLExternSchema zoekObject)
	{
		Criteria crit = maakCriteria(zoekObject);
		crit.setProjection(Projections.countDistinct("id"));
		List<Long> countResult = crit.list();
		return countResult.get(0).longValue();
	}

	@Override
	@SuppressWarnings("unchecked")
	@Transactional(propagation = Propagation.SUPPORTS)
	public int telAantalGekoppeldeBarcodes(SKMLExternSchema zoekObject)
	{
		Criteria crit = getSession().createCriteria(SKMLExterneControleBarcode.class);
		crit.add(Restrictions.eq("schema.id", zoekObject.getId()));
		crit.setProjection(Projections.countDistinct("id"));
		List<Long> countResult = crit.list();
		return countResult.get(0).intValue();
	}

	private Criteria maakCriteria(SKMLExternSchema zoekObject)
	{
		Criteria criteria = getSession().createCriteria(SKMLExternSchema.class);
		if (zoekObject.getRonde() != null)
		{
			criteria.add(Restrictions.eq("ronde", zoekObject.getRonde()));
		}
		if (zoekObject.getLetter() != null)
		{
			criteria.add(Restrictions.eq("letter", zoekObject.getLetter()));
		}
		if (zoekObject.getJaar() != null)
		{
			criteria.add(Restrictions.eq("jaar", zoekObject.getJaar()));
		}
		if (zoekObject.getDeadline() != null)
		{
			criteria.add(Restrictions.eq("deadline", zoekObject.getDeadline()));
		}
		if (zoekObject.getId() != null)
		{
			criteria.add(Restrictions.eq("id", zoekObject.getId()));
		}
		if (zoekObject.getActief() != null)
		{
			criteria.add(Restrictions.eq("actief", zoekObject.getActief()));
		}
		return criteria;
	}

	@Transactional(propagation = Propagation.SUPPORTS)
	@Override
	public SKMLExternSchema haalEerstvolgendeSchemaOp(Date deadline)
	{
		Date exacteDeadline = new DateTime(deadline).withTimeAtStartOfDay().toDate();
		SKMLExternSchema schema = null;
		Criteria criteria = getSession().createCriteria(SKMLExternSchema.class);
		criteria.add(Restrictions.eq("actief", true));
		criteria.add(Restrictions.ge("deadline", exacteDeadline));
		criteria.addOrder(Order.asc("deadline"));
		criteria.setMaxResults(1);
		Object result = criteria.uniqueResult();
		if (result != null)
		{
			schema = (SKMLExternSchema) result;
		}
		return schema;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<SKMLExternSchema> haalSchemasVanafDeadlineDatum(Date deadline)
	{
		Date exacteDeadline = new DateTime(deadline).withTimeAtStartOfDay().toDate();
		List<SKMLExternSchema> resultaten = new ArrayList<>();
		Criteria criteria = getSession().createCriteria(SKMLExternSchema.class);
		criteria.add(Restrictions.ge("deadline", exacteDeadline));
		criteria.add(Restrictions.eq("actief", true));
		resultaten.addAll(criteria.list());
		return resultaten;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<SKMLExternSchema> zoekSchema(SKMLExternSchema schema, boolean alleenOpDeadline)
	{
		Criteria criteria = getSession().createCriteria(SKMLExternSchema.class);
		criteria.add(Restrictions.eq("actief", true));
		if (schema.getDeadline() != null)
		{
			criteria.add(Restrictions.eq("deadline", schema.getDeadline()));
		}
		if (schema.getJaar() != null && !alleenOpDeadline)
		{
			criteria.add(Restrictions.eq("jaar", schema.getJaar()));
		}
		if (StringUtils.isNotBlank(schema.getLetter()) && !alleenOpDeadline)
		{
			criteria.add(Restrictions.eq("letter", schema.getLetter()));
		}
		if (schema.getRonde() != null && !alleenOpDeadline)
		{
			criteria.add(Restrictions.eq("ronde", schema.getRonde()));
		}
		return criteria.list();
	}
}
