package nl.rivm.screenit.batch.jobs.mamma.onderzoek.onderbrokenonderzoeken;

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
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Projection;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.beans.factory.annotation.Autowired;

public class MammaVervolgTeOudeOnderbrokenOnderzoekenReader extends BaseScrollableResultReader
{
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		Criteria criteria = session.createCriteria(MammaDossier.class, "dossier");
		criteria.createAlias("dossier.client", "client");
		criteria.createAlias("client.persoon", "persoon");
		criteria.createAlias("dossier.laatsteScreeningRonde", "ronde");
		criteria.createAlias("ronde.laatsteOnderzoek", "onderzoek");
		criteria.add(Restrictions.lt("onderzoek.creatieDatum", DateUtil.toUtilDate(currentDateSupplier.getLocalDate().minusMonths(6))));
		criteria.add(Restrictions.eq("onderzoek.status", MammaOnderzoekStatus.ONDERBROKEN));
		criteria.add(Restrictions.eq("onderzoek.isDoorgevoerd", true));

		return criteria;
	}

	@Override
	protected Projection getProjection()
	{
		return Projections.property("onderzoek.id");
	}
}
