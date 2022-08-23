package nl.rivm.screenit.batch.jobs.mamma.aftergba.zonderpostcode;

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
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.util.query.ScreenitRestrictions;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.batch.jobs.mamma.aftergba.AfterGbaJobConfiguration.AFTER_GBA_JOB_READER_FETCH_SIZE;

@Component
public class MammaZonderPostcodeReader extends BaseScrollableResultReader
{

	public MammaZonderPostcodeReader()
	{
		super.setFetchSize(AFTER_GBA_JOB_READER_FETCH_SIZE);
	}

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var criteria = session.createCriteria(Client.class);
		criteria.createAlias("mammaDossier", "mammaDossier");
		criteria.createAlias("persoon", "persoon");
		criteria.createAlias("persoon.gbaAdres", "adres");
		criteria.createAlias("adres.gbaGemeente", "gemeente");
		criteria.createAlias("gemeente.screeningOrganisatie", "regio");
		criteria.createAlias("persoon.tijdelijkGbaAdres", "tijdelijkAdres", JoinType.LEFT_OUTER_JOIN);

		ScreenitRestrictions.addClientBaseRestrictions(criteria, "", "persoon.");

		criteria.add(Restrictions.isNull("adres.postcode"));
		criteria.add(
			Restrictions.or(
				Restrictions.and(
					Restrictions.isNotNull("tijdelijkAdres.id"),
					Restrictions.isNull("tijdelijkAdres.postcode")),
				Restrictions.isNull("tijdelijkAdres.id")));
		return criteria;
	}
}
