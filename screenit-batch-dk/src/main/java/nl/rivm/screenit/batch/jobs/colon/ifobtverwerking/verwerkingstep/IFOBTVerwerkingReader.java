
package nl.rivm.screenit.batch.jobs.colon.ifobtverwerking.verwerkingstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.colon.IFOBTUitslag;
import nl.rivm.screenit.model.colon.enums.IFOBTBestandStatus;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projection;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;

public class IFOBTVerwerkingReader extends BaseScrollableResultReader
{

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		try
		{
			Criteria crit = session.createCriteria(IFOBTUitslag.class);

			crit.createAlias("bestand", "bestand");
			crit.add(Restrictions.eq("bestand.status", IFOBTBestandStatus.GEAUTORISEERD));

			crit.addOrder(Order.asc("bestand.id"));
			crit.addOrder(Order.asc("id"));

			return crit;
		}
		catch (Exception e)
		{
			crashMelding("Er konden geen uitslagen worden geselecteerd voor de verwerking", e);
			throw e;
		}
	}

	@Override
	protected Projection getProjection()
	{
		return Projections.id();
	}
}
