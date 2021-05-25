package nl.rivm.screenit.batch.jobs.mamma.aftergba.imswijzigingendoorsturen;

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

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.Client;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Disjunction;
import org.hibernate.criterion.MatchMode;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;

public class MammaImsWijzigingenDoorsturenReader extends BaseScrollableResultReader
{

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		Criteria criteria = session.createCriteria(Client.class);
		criteria.createAlias("gbaMutaties", "gbaMutaties");
		criteria.createAlias("mammaDossier", "mammaDossier", JoinType.INNER_JOIN);
		Disjunction disjunction = Restrictions.disjunction();
		disjunction.add(Restrictions.like("gbaMutaties.aanvullendeInformatie", Constants.MAMMA_IMS_CLIENT_BSN_GEWIJZIGD_MARKER, MatchMode.ANYWHERE));
		disjunction.add(Restrictions.like("gbaMutaties.aanvullendeInformatie", Constants.MAMMA_IMS_CLIENT_GEGEVENS_GEWIJZIGD_MARKER, MatchMode.ANYWHERE));
		criteria.add(disjunction);
		return criteria;
	}
}
