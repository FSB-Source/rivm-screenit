package nl.rivm.screenit.batch.jobs.cervix.uitstel.step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.cervix.CervixUitstel;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.query.ScreenitRestrictions;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.springframework.beans.factory.annotation.Autowired;

public class CervixUitstelReader extends BaseScrollableResultReader
{

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		Criteria crit = session.createCriteria(CervixUitstel.class, "uitstel");
		crit.createAlias("uitstel.screeningRonde", "ronde");
		crit.createAlias("ronde.dossier", "dossier");
		crit.createAlias("dossier.client", "client");
		crit.createAlias("client.persoon", "persoon");

		ScreenitRestrictions.addClientBaseRestrictions(crit, "client", "persoon");

		crit.add(Restrictions.eq("ronde.status", ScreeningRondeStatus.LOPEND));

		crit.add(Restrictions.isNull("uitstel.geannuleerdDatum"));
		crit.add(Restrictions.le("uitstel.uitstellenTotDatum", dateSupplier.getDate()));

		return crit;
	}
}
