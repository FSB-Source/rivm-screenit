package nl.rivm.screenit.batch.jobs.aftergba.retourzendingstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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
import nl.rivm.screenit.model.colon.enums.RetourzendingStatus;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.MatchMode;
import org.hibernate.criterion.Restrictions;

public abstract class BaseRetourzendingReader extends BaseScrollableResultReader
{

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		Criteria crit = session.createCriteria(Client.class);
		crit.createAlias("gbaMutaties", "mutaties");
		crit.add(Restrictions.like("mutaties.aanvullendeInformatie", "|" + getRetoursendingMarker() + "|", MatchMode.ANYWHERE));
		crit.createAlias(getBvoDossierPropertyInClient(), "dossier");
		crit.createAlias("dossier.screeningRondes", "screeningRonde");
		crit.createAlias("screeningRonde.uitnodigingen", "uitnodiging");
		crit.add(Restrictions.eq("uitnodiging.retourzendingStatus", RetourzendingStatus.NIEUWE_GBA_ADRES_AANGEVRAAGD));

		return crit;
	}

	protected abstract String getBvoDossierPropertyInClient();

	protected abstract String getRetoursendingMarker();
}
