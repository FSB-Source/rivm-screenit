package nl.rivm.screenit.batch.jobs.colon.ifobtverwerking.verwerkingstep;

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

import java.util.ArrayList;
import java.util.List;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Root;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.colon.IFOBTUitslag;
import nl.rivm.screenit.model.colon.IFOBTUitslag_;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.model.colon.enums.IFOBTBestandStatus.GEAUTORISEERD;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.colon.ColonFITBestandSpecification.heeftStatus;

@Component
public class IFOBTVerwerkingReader extends BaseSpecificationScrollableResultReader<IFOBTUitslag>
{
	@Override
	protected Specification<IFOBTUitslag> createSpecification()
	{
		return heeftStatus(GEAUTORISEERD).with(IFOBTUitslag_.bestand);
	}

	@Override
	protected List<Order> getOrders(Root<IFOBTUitslag> r, CriteriaBuilder cb)
	{
		var orders = new ArrayList<Order>();
		orders.add(cb.asc(join(r, IFOBTUitslag_.bestand).get(AbstractHibernateObject_.id)));
		orders.add(cb.asc(r.get(AbstractHibernateObject_.id)));
		return orders;
	}

	@Override
	protected Class<?> getResultClass()
	{
		return Object[].class;
	}
}
