package nl.rivm.screenit.batch.jobs.mamma.aftergba.deelnamekansenherzien;

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

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.mamma.MammaDossier;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.MatchMode;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.batch.jobs.mamma.aftergba.AfterGbaJobConfiguration.AFTER_GBA_JOB_READER_FETCH_SIZE;

@Component
public class MammaDeelnamekansenHerzienReader extends BaseScrollableResultReader
{

	public MammaDeelnamekansenHerzienReader()
	{
		super.setFetchSize(AFTER_GBA_JOB_READER_FETCH_SIZE);
	}

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var criteria = session.createCriteria(MammaDossier.class, "dossier");
		criteria.createAlias("dossier.client", "client");
		criteria.createAlias("client.laatsteGbaMutatie", "gbaMutatie");

		criteria.add(Restrictions.like("gbaMutatie.aanvullendeInformatie", Constants.MAMMA_ADRES_GEWIJZIGD_MARKER, MatchMode.ANYWHERE));
		criteria.add(Restrictions.isNotNull("dossier.screeningRondeEvent"));
		return criteria;
	}
}
