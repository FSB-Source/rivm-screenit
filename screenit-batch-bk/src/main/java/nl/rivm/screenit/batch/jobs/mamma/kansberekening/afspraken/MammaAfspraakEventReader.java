package nl.rivm.screenit.batch.jobs.mamma.kansberekening.afspraken;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import nl.rivm.screenit.batch.jobs.mamma.kansberekening.MammaAbstractEventReader;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.hibernate.Criteria;
import org.hibernate.FetchMode;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.beans.factory.annotation.Autowired;

public class MammaAfspraakEventReader extends MammaAbstractEventReader
{
	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Override
	public Criteria getCriteria(StatelessSession session)
	{
		Criteria criteria = session.createCriteria(MammaAfspraak.class, "afspraak");

		criteria.add(Restrictions.in("afspraak.status", MammaAfspraakStatus.NIET_GEANNULEERD));
		criteria.add(Restrictions.gt("afspraak.vanaf", dateSupplier.getDate()));

		return criteria;
	}
}
