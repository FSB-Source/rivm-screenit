package nl.rivm.screenit.batch.jobs.brieven.genereren;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.util.query.ScreenitRestrictions;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projection;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.batch.item.ExecutionContext;

public abstract class AbstractBrievenGenererenReader<B extends Brief> extends BaseScrollableResultReader
{

	protected abstract Long getScreeningOrganisatieId(ExecutionContext context);

	@SuppressWarnings("unchecked")
	protected final Class<B> getBriefClass()
	{
		return (Class<B>) ((ParameterizedType) getClass().getGenericSuperclass()).getActualTypeArguments()[0];
	}

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		ExecutionContext stepContext = getStepExecutionContext();

		Criteria crit = session.createCriteria(getBriefClass());
		crit.createAlias("client", "client");
		crit.createAlias("client.persoon", "persoon");
		crit.createAlias("persoon.gbaAdres", "gbaAdres");
		crit.createAlias("gbaAdres.gbaGemeente", "gemeente");
		crit.createAlias("gemeente.screeningOrganisatie", "screeningOrganisatie");
		crit.add(Restrictions.eq("screeningOrganisatie.id", getScreeningOrganisatieId(stepContext)));

		ScreenitRestrictions.addPersoonBaseRestrictions(crit, "persoon");

		crit.add(Restrictions.eq("gegenereerd", false));
		crit.add(Restrictions.eq("vervangen", false));
		crit.add(Restrictions.eq("tegenhouden", false));
		crit.add(Restrictions.eq("vervangendeProjectBrief", false));

		crit.addOrder(Order.asc("gbaAdres.postcode"));

		crit = additionalRestrictions(crit, stepContext);

		return crit;
	}

	@Override
	protected Projection getProjection()
	{
		return Projections.id();
	}

	protected Criteria additionalRestrictions(Criteria crit, ExecutionContext context)
	{
		return crit;

	}

	protected int getFetchSize()
	{
		return 20;
	}
}
