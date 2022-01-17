package nl.rivm.screenit.batch.jobs.brieven.cleanup;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.lang.reflect.ParameterizedType;

import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.springframework.beans.factory.annotation.Autowired;

public abstract class AbstractBrievenCleanUpReader<MB extends MergedBrieven<?>> extends BaseScrollableResultReader
{
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		try
		{
			Criteria crit = session.createCriteria(getMergedBrievenClass());

			crit.add(Restrictions.eq("verwijderd", Boolean.FALSE));

			crit.add(
				Restrictions.or(Restrictions.isEmpty("brieven"),
					Restrictions.and(
						Restrictions.eq("geprint", Boolean.TRUE),
						Restrictions.le("printDatum", currentDateSupplier.getDateTime().minusDays(getMinimaleBestaanOpFilestore()).toDate()))));
			return crit;
		}
		catch (Exception e)
		{
			crashMelding("Brieven konden niet geselecteerd worden om opgeruimte te worden", e);
			throw e;
		}

	}

	@SuppressWarnings("unchecked")
	protected final Class<MB> getMergedBrievenClass()
	{
		return (Class<MB>) ((ParameterizedType) getClass().getGenericSuperclass()).getActualTypeArguments()[0];
	}

	protected int getMinimaleBestaanOpFilestore()
	{
		return 14;
	}
}
