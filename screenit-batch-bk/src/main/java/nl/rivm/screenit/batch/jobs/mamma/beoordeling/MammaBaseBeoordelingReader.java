package nl.rivm.screenit.batch.jobs.mamma.beoordeling;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public abstract class MammaBaseBeoordelingReader extends BaseScrollableResultReader
{
	@Autowired
	ICurrentDateSupplier currentDateSupplier;

	protected DetachedCriteria getRadiologenDieAanHetWerkZijn(int uren)
	{
		DetachedCriteria subQueryRadiologenAanHetWerk = DetachedCriteria.forClass(MammaLezing.class);
		subQueryRadiologenAanHetWerk.add(Restrictions.ge("beoordelingDatum", currentDateSupplier.getDateTime().minusHours(uren).toDate()));

		subQueryRadiologenAanHetWerk.setProjection(Projections.distinct(Projections.property("beoordelaar.id")));

		return subQueryRadiologenAanHetWerk;
	}
}
