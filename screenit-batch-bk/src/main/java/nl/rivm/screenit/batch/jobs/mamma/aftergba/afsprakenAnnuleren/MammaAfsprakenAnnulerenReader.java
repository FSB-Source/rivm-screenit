package nl.rivm.screenit.batch.jobs.mamma.aftergba.afsprakenAnnuleren;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.springframework.beans.factory.annotation.Autowired;

public class MammaAfsprakenAnnulerenReader extends BaseScrollableResultReader
{
	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		Criteria criteria = session.createCriteria(Client.class);
		criteria.createAlias("laatsteGbaMutatie", "laatsteGbaMutatie");
		criteria.createAlias("mammaDossier", "mammaDossier");
		criteria.createAlias("mammaDossier.laatsteScreeningRonde", "ronde");
		criteria.createAlias("ronde.laatsteUitnodiging", "uitnodiging");
		criteria.createAlias("uitnodiging.laatsteAfspraak", "afspraak");
		criteria.createAlias("persoon", "persoon");
		criteria.add(Restrictions.or(
			Restrictions.isNotNull("persoon.overlijdensdatum"),
			Restrictions.isNotNull("persoon.datumVertrokkenUitNederland")));
		criteria.add(Restrictions.eq("afspraak.status",
			MammaAfspraakStatus.GEPLAND));
		criteria.add(Restrictions.gt("afspraak.vanaf", dateSupplier.getDate()));
		return criteria;
	}
}
